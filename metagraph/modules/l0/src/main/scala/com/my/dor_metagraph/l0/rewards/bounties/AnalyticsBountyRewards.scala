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
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Balance
import io.constellationnetwork.schema.transaction.RewardTransaction
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
    ): F[RewardTransactionsInformation] = {
      val publicId = deviceInfo.publicId.getOrElse("unknown")
      val teamId = deviceInfo.analyticsBountyInformation.map(_.teamId).getOrElse(UndefinedTeamId)

      deviceInfo.analyticsBountyInformation.get.analyticsRewardAddress match {
        case None =>
          logger.debug(s"[ANALYTICS] Device [publicId=$publicId, teamId=$teamId] doesn't have rewardAddress").as(acc)

        case Some(analyticsRewardAddress) =>
          for {
            (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(devicesBalances, analyticsRewardAddress))

            deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

            deviceTaxToValidatorNodes = validatorNodeTaxOf(deviceTotalRewards)
            rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

            _ <- logger.debug(s"[ANALYTICS] Team representative device [publicId=$publicId, teamId=$teamId, rewardAddress=${analyticsRewardAddress.value.value}] reward=$rewardValue validatorTax=$deviceTaxToValidatorNodes collateralMultiplier=$collateralMultiplierFactor")

            deviceReward = buildDeviceReward(rewardValue, acc.rewardTransactions, analyticsRewardAddress)
            taxesToValidatorNodesUpdated = acc.validatorsTaxes + deviceTaxToValidatorNodes

          } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
      }
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
            val devicePublicIds = teamDevices.flatMap(_.publicId).toList
            analyticsBountyInformation.analyticsRewardAddress match {
              case None => logger.debug(s"[ANALYTICS] Team ${analyticsBountyInformation.teamId} doesn't have default rewardAddress, skipping Analytics rewards. Affected devices=${teamDevices.size}, publicIds=${devicePublicIds.mkString(",")}").as(acc)
              case Some(analyticsRewardAddress) =>
                val teamId = analyticsBountyInformation.teamId
                val devicesCollateralAverage = getDevicesCollateralAverage(teamDevices, acc.lastBalances)
                val newBalancesWithAverage = Map(analyticsRewardAddress -> Balance(devicesCollateralAverage.toNonNegLongUnsafe))

                for {
                  _ <- logger.debug(s"[teamId: $teamId] devices=${teamDevices.size} publicIds=${devicePublicIds.mkString(",")} collateralAverage=$devicesCollateralAverage rewardAddress=$analyticsRewardAddress")
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
    val totalReward = bountyRewards.rewardTransactions.foldLeft(0L)((acc, tx) => acc + tx.amount.value.value)
    for {
      // One aggregate INFO line per cycle; per-payout detail at DEBUG to avoid log spam.
      _ <- logger.info(s"[ANALYTICS] Rewards distributed: payouts=${bountyRewards.rewardTransactions.size} totalReward=$totalReward validatorTax=${bountyRewards.validatorsTaxes}")
      _ <- bountyRewards.rewardTransactions.traverse_(tx =>
        logger.debug(s"[ANALYTICS] reward destination=${tx.destination.value.value} amount=${tx.amount.value.value}")
      )
    } yield ()
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