package com.my.dor_metagraph.l0.rewards.bounties

import cats.effect.Async
import cats.syntax.functor._
import cats.syntax.option.catsSyntaxOptionId
import com.my.dor_metagraph.shared_data.Utils.{PosLongOps, RewardTransactionOps}
import com.my.dor_metagraph.shared_data.types.Types._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Balance
import io.constellationnetwork.schema.transaction.RewardTransaction

abstract class BountyRewards[F[_] : Async] {

  // 10% validator-node tax as exact integer arithmetic (floor). The device keeps `total - tax`, so
  // device reward + validator tax == total exactly (no Double, no minting/burning of datolites).
  protected def validatorNodeTaxOf(total: Long): Long = total / 10

  def logInitialRewardDistribution(
    currentEpochProgress: Long
  ): F[Unit]

  def logAllDevicesRewards(
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit]

  def getBountyRewardsTransactions(
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes]

  def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long

  protected def getDeviceBountiesRewards(
    device                    : DeviceInfo,
    currentEpochProgress      : Long,
    collateralMultiplierFactor: Ratio
  ): F[Long] = {
    for {
      deviceBountiesRewardsAmount <- Async[F].delay(getDeviceBountyRewardsAmount(device, currentEpochProgress))
      rewardsWithCollateral = collateralMultiplierFactor.applyTo(deviceBountiesRewardsAmount)
    } yield rewardsWithCollateral
  }

  protected def buildDeviceReward(
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
}
