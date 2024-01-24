package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

class CommercialLocationBounty extends Bounty {
  override def getBountyRewardAmount(
    deviceInfo  : DorAPIResponse,
    epochModulus: Long
  ): Double = {
    if (epochModulus != 1L) {
      0D
    } else {
      deviceInfo.locationType match {
        case None => 0D
        case Some(storeType) =>
          if (storeType != "Residential") {
            50D
          } else {
            0D
          }
      }
    }

  }
}