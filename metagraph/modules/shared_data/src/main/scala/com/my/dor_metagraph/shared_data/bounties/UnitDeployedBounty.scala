package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class UnitDeployedBounty extends Bounty {
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

