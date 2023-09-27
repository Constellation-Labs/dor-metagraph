package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.DeviceInfoAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

object Bounties {
  @derive(decoder, encoder)
  sealed trait Bounty {

    def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Double
  }

  @derive(decoder, encoder)
  case class UnitDeployedBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Double = {
      if (epochModulus != 0L) {
        return 0D
      }
      
      50D
    }
  }

  @derive(decoder, encoder)
  case class CommercialLocationBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Double = {
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

  @derive(decoder, encoder)
  case class RetailAnalyticsSubscriptionBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Double = {
      if (epochModulus != 2L) {
        return 0D
      }

      deviceInfo.billedAmountMonthly match {
        case None => 0D
        case Some(billedAmountMonthly) =>
          billedAmountMonthly * 25D
      }

    }
  }
}
