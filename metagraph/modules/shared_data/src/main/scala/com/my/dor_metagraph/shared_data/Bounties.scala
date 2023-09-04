package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.DeviceInfoAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

object Bounties {
  @derive(decoder, encoder)
  sealed trait Bounty {

    def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long
  }

  @derive(decoder, encoder)
  case class UnitDeployedBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long = {
      if (epochModulus != 0L) {
        return 0L
      }
      
      50L
    }
  }

  @derive(decoder, encoder)
  case class CommercialLocationBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long = {
      if (epochModulus != 1L) {
        return 0L
      }

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

  @derive(decoder, encoder)
  case class RetailAnalyticsSubscriptionBounty() extends Bounty {
    override def getBountyRewardAmount(deviceInfo: DeviceInfoAPIResponse, epochModulus: Long): Long = {
      if (epochModulus != 2L) {
        return 0L
      }

      deviceInfo.billedAmountMonthly match {
        case None => 0L
        case Some(billedAmountMonthly) =>
          billedAmountMonthly * 25L
      }

    }
  }
}
