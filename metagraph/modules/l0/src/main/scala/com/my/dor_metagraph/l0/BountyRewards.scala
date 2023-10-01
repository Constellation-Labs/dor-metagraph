package com.my.dor_metagraph.l0

import com.my.dor_metagraph.l0.Types.{COLLATERAL_100K, COLLATERAL_200K, COLLATERAL_50K, COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER, COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER, COLLATERAL_GREATER_THAN_200K_MULTIPLIER, COLLATERAL_LESS_THAN_50K_MULTIPLIER}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, RetailAnalyticsSubscriptionBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceInfo, EPOCH_PROGRESS_1_DAY}
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.types.all.PosLong
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}

object BountyRewards {
  val bountyRewards: BountyRewards = BountyRewards()

  def getDeviceBountyRewardsAmount(device: DeviceInfo, currentEpochProgress: Long): Long = {
    bountyRewards.getDeviceBountyRewardsAmount(device, currentEpochProgress)
  }

  def calculateBountiesRewardsWithCollateral(lastBalances: Map[Address, Balance], rewardAddress: Address, deviceTotalRewards: Long): Long = {
    bountyRewards.calculateBountiesRewardsWithCollateral(lastBalances, rewardAddress, deviceTotalRewards)
  }

  def getTaxesToValidatorNodes(deviceTotalRewards: Long): Long = {
    bountyRewards.getTaxesToValidatorNodes(deviceTotalRewards)
  }

  def getBountyRewardsTransactions(state: CheckInState, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): (List[RewardTransaction], Long) = {
    bountyRewards.getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)
  }
}

case class BountyRewards() {

  private val logger = LoggerFactory.getLogger(classOf[BountyRewards])

  private def getDeviceBountiesRewards(device: DeviceInfo, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): Long = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)
    calculateBountiesRewardsWithCollateral(lastBalances, device.deviceApiResponse.rewardAddress, deviceBountiesRewardsAmount)
  }

  def getDeviceBountyRewardsAmount(device: DeviceInfo, currentEpochProgress: Long): Long = {
    val epochModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY

    if (epochModulus == 0L) {
      val tokenAmount = toTokenAmountFormat(UnitDeployedBounty().getBountyRewardAmount(device.deviceApiResponse, epochModulus))
      logger.info(s"[UnitDeployedBounty] Device with rewardAddress: ${device.deviceApiResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }
    if (epochModulus == 1L) {
      val tokenAmount = toTokenAmountFormat(CommercialLocationBounty().getBountyRewardAmount(device.deviceApiResponse, epochModulus))
      logger.info(s"[CommercialLocationBounty] Device with rewardAddress: ${device.deviceApiResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }
    if (epochModulus == 2L) {
      val tokenAmount = toTokenAmountFormat(RetailAnalyticsSubscriptionBounty().getBountyRewardAmount(device.deviceApiResponse, epochModulus))
      logger.info(s"[RetailAnalyticsSubscriptionBounty] Device with rewardAddress: ${device.deviceApiResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }

    0L
  }

  def calculateBountiesRewardsWithCollateral(lastBalances: Map[Address, Balance], rewardAddress: Address, deviceTotalRewards: Long): Long = {
    val updatedBalance = lastBalances.get(rewardAddress) match {
      case Some(rewardAddressBalance) =>
        val balance = rewardAddressBalance.value.value
        if (balance < COLLATERAL_50K) {
          deviceTotalRewards * COLLATERAL_LESS_THAN_50K_MULTIPLIER
        } else if (balance >= COLLATERAL_50K && balance < COLLATERAL_100K) {
          deviceTotalRewards * COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER
        } else if (balance >= COLLATERAL_100K && balance < COLLATERAL_200K) {
          deviceTotalRewards * COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER
        } else {
          deviceTotalRewards * COLLATERAL_GREATER_THAN_200K_MULTIPLIER
        }
      case None => deviceTotalRewards.toDouble
    }

    updatedBalance.toLong
  }

  def getTaxesToValidatorNodes(deviceTotalRewards: Long): Long = {
    (deviceTotalRewards * 0.1).toLong
  }

  def getBountyRewardsTransactions(state: CheckInState, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): (List[RewardTransaction], Long) = {
    var taxesToValidatorNodes = 0L
    val allRewards: Map[Address, RewardTransaction] = Map.empty

    val rewardsTransactions = state.devices.foldLeft(allRewards) { case (acc, (_, value)) =>
      if (currentEpochProgress - value.nextEpochProgressToReward > EPOCH_PROGRESS_1_DAY) {
        logger.warn(s"Device with reward address ${value.deviceApiResponse.rewardAddress.value.value} didn't make a check in in the last 24 hours")
        acc
      } else {
        val deviceTotalRewards = getDeviceBountiesRewards(value, currentEpochProgress, lastBalances)
        val deviceTaxToValidatorNodes = getTaxesToValidatorNodes(deviceTotalRewards)
        val rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes
        taxesToValidatorNodes += deviceTaxToValidatorNodes

        if (rewardValue > 0) {
          acc.get(value.deviceApiResponse.rewardAddress) match {
            case Some(currentReward) =>
              logger.info(s"Device with rewardAddress: ${value.deviceApiResponse.rewardAddress} already have a reward, increasing value")
              val newValue = currentReward.amount.value.value + rewardValue
              logger.info(s"Device with rewardAddress: ${value.deviceApiResponse.rewardAddress} new value: $newValue")
              acc + (value.deviceApiResponse.rewardAddress -> RewardTransaction(value.deviceApiResponse.rewardAddress, TransactionAmount(PosLong.unsafeFrom(newValue))))
            case None =>
              logger.info(s"Device with rewardAddress: ${value.deviceApiResponse.rewardAddress} doesn't have a reward yet, creating with value: $rewardValue")
              acc + (value.deviceApiResponse.rewardAddress -> RewardTransaction(value.deviceApiResponse.rewardAddress, TransactionAmount(PosLong.unsafeFrom(rewardValue))))
          }
        } else {
          logger.info(s"Ignoring reward, value equals to 0")
          acc
        }
      }
    }.values.filter(_ != null).toList

    (rewardsTransactions, taxesToValidatorNodes)
  }
}
