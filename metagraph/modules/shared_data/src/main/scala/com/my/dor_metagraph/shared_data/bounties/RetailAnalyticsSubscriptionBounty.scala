package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

case class RetailAnalyticsSubscriptionBounty() extends Bounty {
  override def getBountyRewardAmount(deviceInfo: DorAPIResponse, epochModulus: Long): Double = {
    if (epochModulus != 2L) {
      return 0D
    }

    deviceInfo.billedAmountMonthly match {
      case None => 0D
      case Some(billedAmountMonthly) =>
        billedAmountMonthly * 25D
    }

  }
}