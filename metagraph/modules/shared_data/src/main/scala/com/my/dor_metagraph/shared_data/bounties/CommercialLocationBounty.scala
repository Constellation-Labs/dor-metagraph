package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(decoder, encoder)
case class CommercialLocationBounty() extends Bounty {
  override def getBountyRewardAmount(deviceInfo: DorAPIResponse, epochModulus: Long): Double = {
    if (epochModulus != 1L) {
      return 0D
    }

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