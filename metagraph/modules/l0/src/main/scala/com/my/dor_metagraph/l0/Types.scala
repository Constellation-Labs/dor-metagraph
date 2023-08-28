package com.my.dor_metagraph.l0

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat

object Types {
  val COLLATERAL_50K: Long = toTokenAmountFormat(50000)
  val COLLATERAL_100K: Long = toTokenAmountFormat(100000)
  val COLLATERAL_200K: Long = toTokenAmountFormat(200000)

  val COLLATERAL_LESS_THAN_50K_MULTIPLIER: Double = 1
  val COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER: Double = 1.05
  val COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER: Double = 1.1
  val COLLATERAL_GREATER_THAN_200K_MULTIPLIER: Double = 1.2
}
