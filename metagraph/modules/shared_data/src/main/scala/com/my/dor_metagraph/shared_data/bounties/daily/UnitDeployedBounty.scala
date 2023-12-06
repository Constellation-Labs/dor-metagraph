package com.my.dor_metagraph.shared_data.bounties.daily

import com.my.dor_metagraph.shared_data.bounties.Bounty
import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

case class UnitDeployedBounty() extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Double = {
    if (epochModulus != 0L) {
      0D
    } else {
      50D
    }
  }
}

