package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(decoder, encoder)
case class UnitDeployedBounty() extends Bounty {
  override def getBountyRewardAmount(deviceInfo: DorAPIResponse, epochModulus: Long): Double = {
    if (epochModulus != 0L) {
      return 0D
    }

    50D
  }
}

