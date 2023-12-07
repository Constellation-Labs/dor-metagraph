package com.my.dor_metagraph.l0.rewards.bounties

import cats.effect.Async
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import com.my.dor_metagraph.l0.rewards.collateral.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils._
import com.my.dor_metagraph.shared_data.bounties.AnalyticsSubscriptionBounty
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class AnalyticsBountyRewards() extends BountyRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("AnalyticsBountyRewards")

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
        case (_, info) if info.analyticsBountyInformation.exists(_.nextEpochProgressToRewardAnalytics == currentEpochProgress) => info
      }

      txnsInfo <- filteredDevices
        .groupBy(_.analyticsBountyInformation.map(_.teamId).getOrElse(0L))
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

      analyticsRewardTransactionsAndValidatorTaxes = RewardTransactionsAndValidatorsTaxes(txnsInfo.rewardTransactions.values.toList, txnsInfo.validatorsTaxes)

      _ <- logAllDevicesRewards(analyticsRewardTransactionsAndValidatorTaxes)
    } yield analyticsRewardTransactionsAndValidatorTaxes
  }

  override def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    device.analyticsBountyInformation.fold(0L) { analyticsBountyInformation =>
      if (analyticsBountyInformation.nextEpochProgressToRewardAnalytics == currentEpochProgress) {
        toTokenAmountFormat(AnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, 0L))
      } else {
        0L
      }
    }
  }

  override def logInitialRewardDistribution[F[_] : Async](
    epochProgressModulus: Long
  ): F[Unit] = logger.info("Starting AnalyticsSubscriptionBounty bounty distribution")

  override def logAllDevicesRewards[F[_] : Async](
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = {
    if (bountyRewards.rewardTransactions.isEmpty) {
      logger.info(s"[ANALYTICS] No commissions to pay on this epochProgress") >> ().pure
    } else {
      def logRewardTransaction: RewardTransaction => F[Unit] = rewardTransaction =>
        logger.info(s"[ANALYTICS] Device Reward Address: ${rewardTransaction.destination}. Amount: ${rewardTransaction.amount}")

      for {
        _ <- logger.info("[ANALYTICS] All rewards to be distributed to devices")
        _ <- bountyRewards.rewardTransactions.traverse_(logRewardTransaction)
        _ <- logger.info(s"[ANALYTICS] Validators taxes to be distributed between validators: ${bountyRewards.validatorsTaxes}")
      } yield ()
    }
  }

}