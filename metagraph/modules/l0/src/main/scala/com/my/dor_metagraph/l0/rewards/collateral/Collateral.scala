package com.my.dor_metagraph.l0.rewards.collateral

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance

object Collateral {
  def getDeviceCollateral(
    lastBalances : Map[Address, Balance],
    rewardAddress: Address
  ): (Map[Address, Balance], Double) = {
    lastBalances.get(rewardAddress) match {
      case None => (lastBalances, CollateralLessThan50KMultiplier)
      case Some(rawBalance) =>
        val balance = rawBalance.value.value
        val (value, collateralMultiplierFactor) = if (balance < Collateral50K) {
          (balance, CollateralLessThan50KMultiplier)
        } else if (balance < Collateral100K) {
          (balance, CollateralBetween50KAnd100KMultiplier)
        } else if (balance < Collateral200K) {
          (balance, CollateralBetween100KAnd200KMultiplier)
        } else {
          (toTokenAmountFormat(200_000), CollateralGreaterThan200KMultiplier)
        }

        val newBalance = NonNegLong.from(balance - value).getOrElse(NonNegLong.MinValue)

        val updatedLastBalances = lastBalances + (rewardAddress -> Balance(newBalance))
        (updatedLastBalances, collateralMultiplierFactor)
    }
  }
}
