package com.my.dor_metagraph.l0.rewards

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import eu.timepit.refined.types.numeric.NonNegLong

object Collateral {
  def getDeviceCollateral(
    lastBalances : Map[Address, Balance],
    rewardAddress: Address
  ): (Map[Address, Balance], Double) = {
    lastBalances.get(rewardAddress) match {
      case None => (lastBalances, COLLATERAL_LESS_THAN_50K_MULTIPLIER)
      case Some(rawBalance) =>
        val balance = rawBalance.value.value
        val (value, collateralMultiplierFactor) = if (balance < COLLATERAL_50K) {
          (balance, COLLATERAL_LESS_THAN_50K_MULTIPLIER)
        } else if (balance >= COLLATERAL_50K && balance < COLLATERAL_100K) {
          (balance, COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER)
        } else if (balance >= COLLATERAL_100K && balance < COLLATERAL_200K) {
          (balance, COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER)
        } else {
          (toTokenAmountFormat(200000), COLLATERAL_GREATER_THAN_200K_MULTIPLIER)
        }

        val newBalance = NonNegLong.from(balance - value) match {
          case Left(_) => NonNegLong.MinValue
          case Right(value) => value
        }

        val updatedLastBalances = lastBalances + (rewardAddress -> Balance(newBalance))
        (updatedLastBalances, collateralMultiplierFactor)
    }
  }
}
