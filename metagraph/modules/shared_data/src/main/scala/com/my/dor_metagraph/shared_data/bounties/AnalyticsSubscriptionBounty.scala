package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

case class AnalyticsSubscriptionBounty() extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Double = {
    deviceInfo.billedAmountMonthly match {
      case None => 0D
      case Some(billedAmountMonthly) =>
        billedAmountMonthly * 25D
    }
  }
}