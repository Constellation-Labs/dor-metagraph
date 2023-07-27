package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

object Bounties {
  @derive(decoder, encoder)
  sealed trait Bounty {
    val name: String

    def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse): Long
  }

  @derive(decoder, encoder)
  case class UnitDeployedBounty(name: String) extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse): Long = {
      50L
    }
  }

  @derive(decoder, encoder)
  case class CommercialLocationBounty(name: String) extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse): Long = {
      deviceInfo.storeType match {
        case Some(storeType) =>
          if (storeType != "Residential") {
            50L
          } else {
            0L
          }
        case None => 0L
      }
    }
  }
}
