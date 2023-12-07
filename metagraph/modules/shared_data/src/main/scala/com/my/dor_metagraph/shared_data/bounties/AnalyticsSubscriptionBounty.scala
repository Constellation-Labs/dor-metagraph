package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

case class AnalyticsSubscriptionBounty() extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Double = {
    deviceInfo.billedAmount match {
      case None => 0D
      case Some(value) =>
        value * 25D
    }
  }
}