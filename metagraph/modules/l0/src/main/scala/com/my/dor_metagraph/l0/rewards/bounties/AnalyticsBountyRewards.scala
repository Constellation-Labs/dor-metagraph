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
      // .get is safe: only devices whose analyticsBountyInformation is defined reach here (filtered
      // in the `collect` below).
      deviceInfo.analyticsBountyInformation.get.analyticsRewardAddress match {
        case None =>
          Async[F].pure(acc)

        case Some(analyticsRewardAddress) =>
          for {
            (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(devicesBalances, analyticsRewardAddress))

            deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

            deviceTaxToValidatorNodes = validatorNodeTaxOf(deviceTotalRewards)
            rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

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
            // Deterministic representative: pick by a stable total key (publicId, then lastCheckIn)
            // rather than relying on Map/collection iteration order. The representative's billedAmount
            // drives the team reward amount, so this choice must be identical on every validator.
            val device: DeviceInfo = teamDevices.toList.sortBy(d => (d.publicId.getOrElse(""), d.lastCheckIn)).head
            val analyticsBountyInformation = device.analyticsBountyInformation.get
            analyticsBountyInformation.analyticsRewardAddress match {
              case None =>
                Async[F].pure(acc)
              case Some(analyticsRewardAddress) =>
                val devicesCollateralAverage = getDevicesCollateralAverage(teamDevices, acc.lastBalances)
                val newBalancesWithAverage = Map(analyticsRewardAddress -> Balance(devicesCollateralAverage.toNonNegLongUnsafe))
                combine(acc, device, newBalancesWithAverage)
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
    // One aggregate line per reward cycle (no per-payout spam, INFO level only).
    logger.info(s"[ANALYTICS] Rewards distributed: payouts=${bountyRewards.rewardTransactions.size} totalReward=$totalReward validatorTax=${bountyRewards.validatorsTaxes}")
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