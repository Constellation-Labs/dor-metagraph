package com.my.dor_metagraph.l0.rewards.bounties

import cats.effect.Async
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.foldable._
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.l0.rewards.collateral.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils._
import com.my.dor_metagraph.shared_data.bounties.AnalyticsSubscriptionBounty
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class AnalyticsBountyRewards[F[_] : Async] extends BountyRewards {
  def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("AnalyticsBountyRewards")

  override def getBountyRewardsTransactions(
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = {
    def combine(
      acc            : RewardTransactionsInformation,
      deviceInfo     : DeviceInfo,
      devicesBalances: Map[Address, Balance]
    ): F[RewardTransactionsInformation] =
      deviceInfo.analyticsBountyInformation.get.analyticsRewardAddress match {
        case None =>
          logger.warn(s"Device doesn't have rewardAddress").as(acc)

        case Some(analyticsRewardAddress) =>
          for {
            (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(devicesBalances, analyticsRewardAddress))

            deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

            deviceTaxToValidatorNodes = (deviceTotalRewards * ValidatorNodeTaxRate).toLong
            rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

            deviceReward = buildDeviceReward(rewardValue, acc.rewardTransactions, analyticsRewardAddress)
            taxesToValidatorNodesUpdated = acc.validatorsTaxes + deviceTaxToValidatorNodes

          } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
      }

    for {
      _ <- logInitialRewardDistribution(currentEpochProgress)

      groupedDevices = state.devices.collect {
          case (_, info) if info.analyticsBountyInformation.exists(_.nextEpochProgressToRewardAnalytics == currentEpochProgress) => info
        }
        .groupBy(_.analyticsBountyInformation.map(_.teamId).getOrElse(UndefinedTeamId))
        .removed(UndefinedTeamId)

      analyticsRewardTransactionsAndValidatorTaxes <- groupedDevices
        .values
        .toList
        .foldLeftM(RewardTransactionsInformation(Map.empty, 0L, lastBalancesRaw)) { (acc, teamDevices) =>
          if (teamDevices.isEmpty) {
            acc.pure[F]
          } else {
            val device: DeviceInfo = teamDevices.head
            val analyticsBountyInformation = device.analyticsBountyInformation.get
            analyticsBountyInformation.analyticsRewardAddress match {
              case None => logger.warn(s"Team ${analyticsBountyInformation.teamId} doesn't have default rewardAddress, skipping Analytics rewards.").as(acc)
              case Some(analyticsRewardAddress) =>
                val teamId = analyticsBountyInformation.teamId
                val devicesCollateralAverage = getDevicesCollateralAverage(teamDevices, acc.lastBalances)
                val newBalancesWithAverage = Map(analyticsRewardAddress -> Balance(devicesCollateralAverage.toNonNegLongUnsafe))

                for {
                  _ <- logger.info(s"[teamId: $teamId] Devices number: ${teamDevices.size}")
                  _ <- logger.info(s"[teamId: $teamId] Collateral average: $devicesCollateralAverage")
                  _ <- logger.info(s"[teamId: $teamId] Address to be rewarded: $analyticsRewardAddress")
                  rewardTransactionsInformation <- combine(acc, device, newBalancesWithAverage)
                } yield rewardTransactionsInformation
            }
          }
        }
        .map(info => RewardTransactionsAndValidatorsTaxes(info.rewardTransactions.values.toList, info.validatorsTaxes))

      _ <- logAllDevicesRewards(analyticsRewardTransactionsAndValidatorTaxes)
    } yield analyticsRewardTransactionsAndValidatorTaxes
  }

  override def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    device.analyticsBountyInformation.fold(0L) { analyticsBountyInformation =>
      if (analyticsBountyInformation.nextEpochProgressToRewardAnalytics == currentEpochProgress) {
        toTokenAmountFormat(new AnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, 0L))
      } else {
        0L
      }
    }
  }

  override def logInitialRewardDistribution(
    epochProgressModulus: Long
  ): F[Unit] = logger.info(s"Starting AnalyticsSubscriptionBounty distribution for epochProgressModulus $epochProgressModulus")

  override def logAllDevicesRewards(
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = {
    if (bountyRewards.rewardTransactions.isEmpty) {
      logger.info(s"[ANALYTICS] No commissions to pay on this epochProgress").as(())
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

  private def getDevicesCollateralAverage(devices: Iterable[DeviceInfo], balances: Map[Address, Balance]): Long =
    if (devices.isEmpty) {
      0L
    } else {
      val sumOfBalances: Long =
        devices
          .flatMap(_.dorAPIResponse.rewardAddress)
          .flatMap(balances.get)
          .map(_.value.value)
          .sum
      sumOfBalances / devices.size
    }
}