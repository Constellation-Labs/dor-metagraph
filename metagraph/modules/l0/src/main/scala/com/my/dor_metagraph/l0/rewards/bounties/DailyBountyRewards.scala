package com.my.dor_metagraph.l0.rewards.bounties

import cats.effect.Async
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.foldable._
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.l0.rewards.collateral.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils._
import com.my.dor_metagraph.shared_data.bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class DailyBountyRewards[F[_] : Async] extends BountyRewards {
  def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("DailyBountyRewards")

  override def getBountyRewardsTransactions(
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = {
    def noCheckInMade(nextEpochProgressToReward: Long): Boolean =
      currentEpochProgress - nextEpochProgressToReward >= EpochProgress1Day

    def combine(acc: RewardTransactionsInformation, deviceInfo: DeviceInfo): F[RewardTransactionsInformation] =
      deviceInfo.dorAPIResponse.rewardAddress match {
        case None =>
          logger.warn(s"Device doesn't have rewardAddress").as(acc)

        case Some(rewardAddress) if noCheckInMade(deviceInfo.nextEpochProgressToReward) =>
          logger.warn(s"Device with reward address ${rewardAddress.value.value} didn't make a check in the last 24 hours").as(acc)

        case Some(rewardAddress) =>
          for {
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
      bountyRewards <- state.devices.values.toList
        .foldLeftM(RewardTransactionsInformation(Map.empty, 0L, lastBalancesRaw))(combine)
        .map(reward => RewardTransactionsAndValidatorsTaxes(reward.rewardTransactions.values.toList, reward.validatorsTaxes))
      _ <- logAllDevicesRewards(bountyRewards)
    } yield bountyRewards
  }

  override def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    val epochModulus = currentEpochProgress % EpochProgress1Day
    epochModulus match {
      case ModulusInstallationBounty => toTokenAmountFormat(new UnitDeployedBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      case ModulusCommercialBounty => toTokenAmountFormat(new CommercialLocationBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      case _ => 0L
    }
  }

  override def logInitialRewardDistribution(
    currentEpochProgress: Long
  ): F[Unit] = {
    val epochProgressModulus = currentEpochProgress % EpochProgress1Day
    epochProgressModulus match {
      case ModulusInstallationBounty => logger.info("Starting UnitDeployed bounty distribution")
      case ModulusCommercialBounty => logger.info("Starting CommercialLocation bounty distribution")
      case _ => logger.info(s"Invalid epochProgressModulus $epochProgressModulus")
    }
  }

  override def logAllDevicesRewards(
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = {
    def logRewardTransaction: RewardTransaction => F[Unit] = rewardTransaction =>
      logger.info(s"[DAILY] Device Reward Address: ${rewardTransaction.destination}. Amount: ${rewardTransaction.amount}")

    for {
      _ <- logger.info("[DAILY] All rewards to be distributed to devices")
      _ <- bountyRewards.rewardTransactions.traverse_(logRewardTransaction)
      _ <- logger.info(s"[DAILY] Validators taxes to be distributed between validators: ${bountyRewards.validatorsTaxes}")
    } yield ()
  }
}