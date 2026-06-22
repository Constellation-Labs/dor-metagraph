package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class CommercialLocationBounty extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Long = {
    if (epochModulus != 1L) {
      0L
    } else {
      deviceInfo.locationType match {
        case None => 0L
        case Some(storeType) =>
          if (storeType != "Residential") {
            50L
          } else {
            0L
          }
      }
    }

  }
}