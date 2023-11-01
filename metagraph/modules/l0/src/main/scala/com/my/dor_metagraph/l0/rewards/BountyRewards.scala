package com.my.dor_metagraph.l0.rewards

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.bounties.{CommercialLocationBounty, RetailAnalyticsSubscriptionBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.types.all.PosLong
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}

object BountyRewards {
  private val logger = LoggerFactory.getLogger("BountyRewards")

  private def getDeviceBountiesRewards(device: DeviceInfo, currentEpochProgress: Long, collateralMultiplierFactor: Double): Long = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)
    val rewardsWithCollateral = deviceBountiesRewardsAmount * collateralMultiplierFactor

    rewardsWithCollateral.toLong
  }

  def getDeviceBountyRewardsAmount(device: DeviceInfo, currentEpochProgress: Long): Long = {
    val epochModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY

    if (epochModulus == 0L) {
      val tokenAmount = toTokenAmountFormat(UnitDeployedBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      logger.info(s"[UnitDeployedBounty] Device with rewardAddress: ${device.dorAPIResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }
    if (epochModulus == 1L) {
      val tokenAmount = toTokenAmountFormat(CommercialLocationBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      logger.info(s"[CommercialLocationBounty] Device with rewardAddress: ${device.dorAPIResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }
    if (epochModulus == 2L) {
      val tokenAmount = toTokenAmountFormat(RetailAnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
      logger.info(s"[RetailAnalyticsSubscriptionBounty] Device with rewardAddress: ${device.dorAPIResponse.rewardAddress}. Reward raw amount: $tokenAmount")
      return tokenAmount
    }

    0L
  }

  def getTaxesToValidatorNodes(deviceTotalRewards: Long): Long = {
    (deviceTotalRewards * 0.1).toLong
  }

  def getBountyRewardsTransactions(state: CheckInDataCalculatedState, currentEpochProgress: Long, lastBalancesRaw: Map[Address, Balance]): (List[RewardTransaction], Long) = {
    var taxesToValidatorNodes = 0L
    val allRewards: Map[Address, RewardTransaction] = Map.empty
    var lastBalances = lastBalancesRaw.map { case (address, balance) => address -> balance.value.value }

    val rewardsTransactions = state.devices.foldLeft(allRewards) { case (acc, (_, value)) =>
      value.dorAPIResponse.rewardAddress match {
        case None =>
          logger.warn(s"Device doesn't have rewardAddress")
          acc
        case Some(rewardAddress) =>
          if (currentEpochProgress - value.nextEpochProgressToReward > EPOCH_PROGRESS_1_DAY) {
            logger.warn(s"Device with reward address ${rewardAddress.value.value} didn't make a check in in the last 24 hours")
            acc
          } else {
            val deviceCollateral = getDeviceCollateral(lastBalances, rewardAddress)
            lastBalances = deviceCollateral._1
            val collateralMultiplierFactor = deviceCollateral._2

            val deviceTotalRewards = getDeviceBountiesRewards(value, currentEpochProgress, collateralMultiplierFactor)
            val deviceTaxToValidatorNodes = getTaxesToValidatorNodes(deviceTotalRewards)
            val rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes
            taxesToValidatorNodes += deviceTaxToValidatorNodes

            if (rewardValue > 0) {
              acc.get(rewardAddress) match {
                case Some(currentReward) =>
                  logger.info(s"Device with rewardAddress: ${value.dorAPIResponse.rewardAddress} already have a reward, increasing value")
                  val newValue = currentReward.amount.value.value + rewardValue
                  logger.info(s"Device with rewardAddress: ${value.dorAPIResponse.rewardAddress} new value: $newValue")
                  acc + (rewardAddress -> RewardTransaction(rewardAddress, TransactionAmount(PosLong.unsafeFrom(newValue))))
                case None =>
                  logger.info(s"Device with rewardAddress: ${value.dorAPIResponse.rewardAddress} doesn't have a reward yet, creating with value: $rewardValue")
                  acc + (rewardAddress -> RewardTransaction(rewardAddress, TransactionAmount(PosLong.unsafeFrom(rewardValue))))
              }
            } else {
              logger.info(s"Ignoring reward, value equals to 0")
              acc
            }
          }
      }
    }.values.filter(_ != null).toList

    (rewardsTransactions, taxesToValidatorNodes)
  }

  def getDeviceCollateral(lastBalances: Map[Address, Long], rewardAddress: Address): (Map[Address, Long], Double) = {
    lastBalances.get(rewardAddress) match {
      case None => (lastBalances, COLLATERAL_LESS_THAN_50K_MULTIPLIER)
      case Some(balance) =>
        val (value, collateralMultiplierFactor) = if (balance < COLLATERAL_50K) {
          (balance, COLLATERAL_LESS_THAN_50K_MULTIPLIER)
        } else if (balance >= COLLATERAL_50K && balance < COLLATERAL_100K) {
          (balance, COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER)
        } else if (balance >= COLLATERAL_100K && balance < COLLATERAL_200K) {
          (balance, COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER)
        } else {
          (toTokenAmountFormat(200000), COLLATERAL_GREATER_THAN_200K_MULTIPLIER)
        }

        val updatedBalance = balance - value
        val updatedLastBalances = lastBalances + (rewardAddress -> updatedBalance)

        (updatedLastBalances, collateralMultiplierFactor)
    }
  }
}