package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class AnalyticsSubscriptionBounty extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Long =
    deviceInfo.billedAmount.fold(0L)(billed => Math.multiplyExact(billed, 25L))
}