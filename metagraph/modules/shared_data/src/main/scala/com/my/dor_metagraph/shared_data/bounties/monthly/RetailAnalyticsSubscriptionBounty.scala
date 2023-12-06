package com.my.dor_metagraph.shared_data.bounties.monthly

import com.my.dor_metagraph.shared_data.bounties.Bounty
import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

case class RetailAnalyticsSubscriptionBounty() extends Bounty {
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