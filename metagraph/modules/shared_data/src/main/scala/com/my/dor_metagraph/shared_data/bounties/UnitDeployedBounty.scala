package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class UnitDeployedBounty extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Long = {
    if (epochModulus != 0L) {
      0L
    } else {
      50L
    }
  }
}

