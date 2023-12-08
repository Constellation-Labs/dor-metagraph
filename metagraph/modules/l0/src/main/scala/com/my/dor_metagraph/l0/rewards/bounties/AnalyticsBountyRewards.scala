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

object AnalyticsBountyRewards {
  def make[F[_] : Async]: BountyRewards[F] =
    new BountyRewards {
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
          deviceInfo.dorAPIResponse.rewardAddress match {
            case None =>
              logger.warn(s"Device doesn't have rewardAddress").as(acc)

            case Some(rewardAddress) =>
              for {
                (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(devicesBalances, rewardAddress))

                deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

                deviceTaxToValidatorNodes = (deviceTotalRewards * ValidatorNodeTaxRate).toLong
                rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

                deviceReward = buildDeviceReward(rewardValue, acc.rewardTransactions, rewardAddress)
                taxesToValidatorNodesUpdated = acc.validatorsTaxes + deviceTaxToValidatorNodes

              } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
          }

        for {
          _ <- logInitialRewardDistribution(currentEpochProgress)

          groupedDevices = state.devices.collect {
              case (_, info) if info.analyticsBountyInformation.exists(_.nextEpochProgressToRewardAnalytics == currentEpochProgress) => info
            }
            .groupBy(_.analyticsBountyInformation.map(_.teamId).getOrElse(UndefinedTeamId))
            .filter(_._1 != UndefinedTeamId)

          analyticsRewardTransactionsAndValidatorTaxes <- groupedDevices
            .toList
            .foldLeftM(RewardTransactionsInformation(Map.empty, 0L, lastBalancesRaw)) { (acc, devices) =>
              val teamDevices = devices._2.toList
              if (teamDevices.isEmpty) {
                acc.pure[F]
              } else {
                teamDevices
                  .find(_.dorAPIResponse.rewardAddress.isDefined)
                  .fold(acc.pure[F]) { deviceToPayCommissions =>
                    val rewardAddress = deviceToPayCommissions.dorAPIResponse.rewardAddress.get
                    val teamId = deviceToPayCommissions.analyticsBountyInformation.get.teamId

                    val devicesCollateralAverage = getDevicesCollateralAverage(teamDevices, acc.lastBalances)
                    val newBalancesWithAverage = Map(rewardAddress -> Balance(devicesCollateralAverage.toNonNegLongUnsafe))

                    for {
                      _ <- logger.info(s"[teamId: $teamId] Devices number: ${teamDevices.size}")
                      _ <- logger.info(s"[teamId: $teamId] Collateral average: $devicesCollateralAverage")
                      _ <- logger.info(s"[teamId: $teamId] Address to be rewarded: ${rewardAddress.value.value}")
                      rewardTransactionsInformation <- combine(acc, deviceToPayCommissions, newBalancesWithAverage)
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
            toTokenAmountFormat(AnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, 0L))
          } else {
            0L
          }
        }
      }

      override def logInitialRewardDistribution(
        epochProgressModulus: Long
      ): F[Unit] = logger.info("Starting AnalyticsSubscriptionBounty bounty distribution")

      override def logAllDevicesRewards(
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

  private def getDevicesCollateralAverage(
    devices : List[DeviceInfo],
    balances: Map[Address, Balance]
  ): Long = {
    val devicesBalances = devices.map { device =>
      device.dorAPIResponse.rewardAddress.fold(0L) { rewardAddress =>
        balances.get(rewardAddress)
          .map(_.value.value)
          .getOrElse(0L)
      }
    }.sum

    devicesBalances / devices.size
  }
}