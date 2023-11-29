package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import com.my.dor_metagraph.l0.rewards.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils._
import com.my.dor_metagraph.shared_data.bounties._
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.SelfAwareStructuredLogger

object BountyRewards {
  //Validator nodes should have 10%
  private val ValidatorNodeTaxPercent: Double = 10D / 100

  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("BountyRewards")

  def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    val epochModulus = currentEpochProgress % EpochProgress1Day
    epochModulus match {
      case 0L => toTokenAmountFormat(UnitDeployedBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      case 1L => toTokenAmountFormat(CommercialLocationBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      case 2L => toTokenAmountFormat(RetailAnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      case _ => 0L
    }
  }

  private def getDeviceBountiesRewards[F[_] : Async](
    device                    : DeviceInfo,
    currentEpochProgress      : Long,
    collateralMultiplierFactor: Double
  ): F[Long] = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)
    val rewardsWithCollateral = (deviceBountiesRewardsAmount * collateralMultiplierFactor).toLong
    logger.info(s"Device with reward address ${device.dorAPIResponse.rewardAddress}. Raw amount: $deviceBountiesRewardsAmount. Amount with collateral: ${rewardsWithCollateral}")
      .as(rewardsWithCollateral)
  }

  private def buildDeviceReward(
    rawRewardValue: Long,
    acc           : Map[Address, RewardTransaction],
    rewardAddress : Address
  ): Map[Address, RewardTransaction] =
    acc.updatedWith(rewardAddress) {
      _.map(_.amount.value.value + rawRewardValue)
        .orElse(rawRewardValue.some)
        .filter(v => v > 0L)
        .map(v =>
          (rewardAddress, v.toPosLongUnsafe).toRewardTransaction
        )
    }

  def getBountyRewardsTransactions[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = {
    def noCheckMade(nextEpochProgressToReward: Long): Boolean =
      currentEpochProgress - nextEpochProgressToReward > EpochProgress1Day

    def combine(acc: RewardTransactionsInformation, deviceInfo: DeviceInfo): F[RewardTransactionsInformation] =
      deviceInfo.dorAPIResponse.rewardAddress match {
        case None =>
          logger.warn(s"Device doesn't have rewardAddress").as(acc)

        case Some(rewardAddress) if noCheckMade(deviceInfo.nextEpochProgressToReward) =>
          logger.warn(s"Device with reward address ${rewardAddress.value.value} didn't make a check in the last 24 hours").as(acc)

        case Some(rewardAddress) =>
          for {
            (updatedBalances, collateralMultiplierFactor) <- Async[F].delay(getDeviceCollateral(acc.lastBalances, rewardAddress))

            deviceTotalRewards <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)

            deviceTaxToValidatorNodes = (deviceTotalRewards * ValidatorNodeTaxPercent).toLong
            rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

            deviceReward = buildDeviceReward(rewardValue, acc.rewardTransactions, rewardAddress)
            taxesToValidatorNodesUpdated = acc.validatorsTaxes + deviceTaxToValidatorNodes

          } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
      }

    val empty = RewardTransactionsInformation(Map.empty, 0L, lastBalancesRaw)
    state.devices.values.toList
      .foldLeftM(empty)(combine)
      .map(reward => RewardTransactionsAndValidatorsTaxes(reward.rewardTransactions.values.toList, reward.validatorsTaxes))
  }
}