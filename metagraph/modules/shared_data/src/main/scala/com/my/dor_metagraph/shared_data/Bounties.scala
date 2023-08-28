package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

object Bounties {
  @derive(decoder, encoder)
  sealed trait Bounty {
    val name: String

    def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long
  }

  @derive(decoder, encoder)
  case class UnitDeployedBounty(name: String) extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long = {
      if (epochModulus != 0L) {
        return 0L
      }
      
      50L
    }
  }

  @derive(decoder, encoder)
  case class CommercialLocationBounty(name: String) extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long = {
      if (epochModulus != 1L) {
        return 0L
      }

      deviceInfo.storeType match {
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
