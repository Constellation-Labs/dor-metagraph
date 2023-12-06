package com.my.dor_metagraph.l0.rewards.bounty

import cats.effect.Async
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.applicative.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.Utils.{PosLongOps, RewardTransactionOps}
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction
import cats.syntax.functor._

abstract class BountyRewards() {

  val ValidatorNodeTaxRate: Double = 0.10

  def logInitialRewardDistribution[F[_] : Async](
    currentEpochProgress: Long
  ): F[Unit] = ().pure[F]

  def logAllDevicesRewards[F[_] : Async](
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = ().pure[F]

  def getBountyRewardsTransactions[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = RewardTransactionsAndValidatorsTaxes.empty.pure[F]

  def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = 0L

  protected def getDeviceBountiesRewards[F[_] : Async](
    device                    : DeviceInfo,
    currentEpochProgress      : Long,
    collateralMultiplierFactor: Double
  ): F[Long] = {
    for {
      deviceBountiesRewardsAmount <- Async[F].delay(getDeviceBountyRewardsAmount(device, currentEpochProgress))
      rewardsWithCollateral = (deviceBountiesRewardsAmount * collateralMultiplierFactor).toLong
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
