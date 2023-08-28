package com.my.dor_metagraph.l0

import com.my.dor_metagraph.l0.Types.{COLLATERAL_100K, COLLATERAL_200K, COLLATERAL_50K, COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER, COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER, COLLATERAL_GREATER_THAN_200K_MULTIPLIER, COLLATERAL_LESS_THAN_50K_MULTIPLIER}
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceInfo, EPOCH_PROGRESS_1_DAY}
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.types.all.PosLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}

object BountyRewards {
  def getDeviceBountyRewardsAmount(device: DeviceInfo, currentEpochProgress: Long): Long = {
    val epochModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY
    var deviceTotalRewards = 0L

    for (bounty <- device.bounties) {
      deviceTotalRewards += bounty.getBountyRewardAmount(device.deviceApiResponse, epochModulus)
    }

    toTokenAmountFormat(deviceTotalRewards.toDouble)
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

  private def getDeviceBountiesRewards(device: DeviceInfo, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): Long = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)
    calculateBountiesRewardsWithCollateral(lastBalances, device.deviceApiResponse.rewardAddress, deviceBountiesRewardsAmount)
  }

  def getTaxesToValidatorNodes(deviceTotalRewards: Long): Long = {
    (deviceTotalRewards * 0.1).toLong
  }

  def getBountyRewardsTransactions(state: CheckInState, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): (List[RewardTransaction], Long) = {
    var taxesToValidatorNodes = 0L
    val rewardsTransactions = state.devices.map { case (_, value) =>
      if (currentEpochProgress - value.nextEpochProgressToReward > EPOCH_PROGRESS_1_DAY) {
        println(s"Device with reward address ${value.deviceApiResponse.rewardAddress.value.value} didn't make a check in in the last 24 hours")
        null
      } else {
        val deviceTotalRewards = getDeviceBountiesRewards(value, currentEpochProgress, lastBalances)
        val deviceTaxToValidatorNodes = getTaxesToValidatorNodes(deviceTotalRewards)
        val rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

        taxesToValidatorNodes += deviceTaxToValidatorNodes
        if (rewardValue > 0) {
          //We already checked if the rewardValue is greater than 0, that's the reason we call unsafeFrom
          RewardTransaction(value.deviceApiResponse.rewardAddress, TransactionAmount(PosLong.unsafeFrom(rewardValue)))
        } else {
          null
        }
      }
    }.filter(_ != null).toList

    (rewardsTransactions, taxesToValidatorNodes)
  }
}
