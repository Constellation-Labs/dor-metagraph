package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class AnalyticsSubscriptionBounty extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Double =
    deviceInfo.billedAmount.map(_ * 25.0).getOrElse(0.0)
}