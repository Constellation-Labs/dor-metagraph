package com.my.dor_metagraph.l0.rewards.bounty

import cats.effect.Async
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import com.my.dor_metagraph.l0.rewards.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils._
import com.my.dor_metagraph.shared_data.bounties.monthly.RetailAnalyticsSubscriptionBounty
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class MonthlyBountyRewards() extends BountyRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("MonthlyBountyRewards")

  override def getBountyRewardsTransactions[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = {
    def combine(acc: RewardTransactionsInformation, deviceInfo: DeviceInfo): F[RewardTransactionsInformation] =
      deviceInfo.dorAPIResponse.rewardAddress match {
        case None =>
          logger.warn(s"Device doesn't have rewardAddress").as(acc)

        case Some(rewardAddress) =>
          for {
            //Remember to ask about this
            (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(acc.lastBalances, rewardAddress))

            deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

            deviceTaxToValidatorNodes = (deviceTotalRewards * ValidatorNodeTaxRate).toLong
            rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

            deviceReward = buildDeviceReward(rewardValue, acc.rewardTransactions, rewardAddress)
            taxesToValidatorNodesUpdated = acc.validatorsTaxes + deviceTaxToValidatorNodes

          } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
      }

    for {
      _ <- logInitialRewardDistribution(currentEpochProgress)

      filteredDevices = state.devices.collect {
        case (_, info) if info.retailBountyInformation.exists(_.nextEpochProgressToRewardRetail == currentEpochProgress) => info
      }

      txnsInfo <- filteredDevices
        .groupBy(_.retailBountyInformation.map(_.teamId).getOrElse(0L))
        .filter(_._1 != 0L)
        .toList
        .foldLeftM(RewardTransactionsInformation(Map.empty, 0L, lastBalancesRaw)) { (acc, devices) =>
          val teamDevices = devices._2.toList
          if (teamDevices.isEmpty) {
            acc.pure[F]
          } else {
            combine(acc, teamDevices.head)
          }
        }

      monthlyRewardTransactionsAndValidatorTaxes = RewardTransactionsAndValidatorsTaxes(txnsInfo.rewardTransactions.values.toList, txnsInfo.validatorsTaxes)

      _ <- logAllDevicesRewards(monthlyRewardTransactionsAndValidatorTaxes)
    } yield monthlyRewardTransactionsAndValidatorTaxes
  }

  override def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    device.retailBountyInformation.fold(0L) { retailBountyInformation =>
      if (retailBountyInformation.nextEpochProgressToRewardRetail == currentEpochProgress) {
        toTokenAmountFormat(RetailAnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, 0L))
      } else {
        0L
      }
    }
  }

  override def logInitialRewardDistribution[F[_] : Async](
    epochProgressModulus: Long
  ): F[Unit] = logger.info("Starting RetailAnalyticsSubscription bounty distribution")

  override def logAllDevicesRewards[F[_] : Async](
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = {
    if (bountyRewards.rewardTransactions.isEmpty) {
      logger.info(s"[MONTHLY] No commissions to pay monthly on this epochProgress") >> ().pure
    } else {
      def logRewardTransaction: RewardTransaction => F[Unit] = rewardTransaction =>
        logger.info(s"[MONTHLY] Device Reward Address: ${rewardTransaction.destination}. Amount: ${rewardTransaction.amount}")

      for {
        _ <- logger.info("[MONTHLY] All rewards to be distributed to devices")
        _ <- bountyRewards.rewardTransactions.traverse_(logRewardTransaction)
        _ <- logger.info(s"[MONTHLY] Validators taxes to be distributed between validators: ${bountyRewards.validatorsTaxes}")
      } yield ()
    }
  }

}