package com.my.dor_metagraph.shared_data.combiners

import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import org.tessellation.schema.epoch.EpochProgress

object DeviceCheckIn {
  def combineDeviceCheckIn(
    acc          : DataState[CheckInStateOnChain, CheckInDataCalculatedState],
    checkInUpdate: CheckInUpdate,
    address      : Address,
    epochProgress: EpochProgress
  ): DataState[CheckInStateOnChain, CheckInDataCalculatedState] = {
    val maybeDeviceInfo = acc.calculated.devices.get(address)
    val dorAPIResponse: DorAPIResponse = checkInUpdate.maybeDorAPIResponse.get

    val nextRewardEpochProgress: Long = nextEpochProgressToReward(epochProgress, maybeDeviceInfo)
    val maybeRetailBountyInformation: Option[RetailBountyInformation] = getRetailRewardsInformation(epochProgress, maybeDeviceInfo, dorAPIResponse)

    val checkInProof = CheckInProof(checkInUpdate.publicId, checkInUpdate.signature)
    val checkInStateUpdate = CheckInStateUpdate(address, checkInUpdate.dts, checkInProof, checkInUpdate.dtmCheckInHash)

    val checkIn = DeviceInfo(checkInUpdate.dts, dorAPIResponse, nextRewardEpochProgress, maybeRetailBountyInformation)

    val devices: Map[Address, DeviceInfo] = acc.calculated.devices.updated(address, checkIn)
    val updates: List[CheckInStateUpdate] = checkInStateUpdate :: acc.onChain.updates

    DataState(
      CheckInStateOnChain(updates),
      CheckInDataCalculatedState(devices)
    )
  }

  private def nextEpochProgressToReward(
    epochProgress  : EpochProgress,
    maybeDeviceInfo: Option[DeviceInfo]
  ): Long = {
    val currentEpoch: Long = epochProgress.value.value
    val currentEpochModulus: Long = currentEpoch % EpochProgress1Day
    val nextRewardEpoch: Long = currentEpoch - currentEpochModulus + EpochProgress1Day

    maybeDeviceInfo
      .filter(_.nextEpochProgressToReward > currentEpoch)
      .map(_.nextEpochProgressToReward)
      .getOrElse(nextRewardEpoch)
  }

  private def getRetailRewardsInformation(
    epochProgress  : EpochProgress,
    maybeDeviceInfo: Option[DeviceInfo],
    dorAPIResponse : DorAPIResponse
  ): Option[RetailBountyInformation] =
    dorAPIResponse.lastBillingId.map { lastBillingId =>
      maybeDeviceInfo.fold(createFirstRetailBountyInformation(epochProgress, dorAPIResponse)) { deviceInfo =>
        deviceInfo.retailBountyInformation.fold(createFirstRetailBountyInformation(epochProgress, dorAPIResponse)) { oldRetailBountyInformation =>
          updateRetailBountyInformation(epochProgress, dorAPIResponse, lastBillingId, oldRetailBountyInformation)
        }
      }
    }

  private def createFirstRetailBountyInformation(
    epochProgress : EpochProgress,
    dorAPIResponse: DorAPIResponse
  ): RetailBountyInformation = {
    val currentEpoch: Long = epochProgress.value.value
    val currentEpochModulus: Long = currentEpoch % EpochProgress1Day
    val nextRewardEpoch: Long = currentEpoch - currentEpochModulus + EpochProgress1Day + OffsetRetailBounty

    RetailBountyInformation(
      nextRewardEpoch,
      dorAPIResponse.teamId.get,
      dorAPIResponse.lastBillingId.get,
      dorAPIResponse.billedAmountMonthly.get
    )
  }

  private def updateRetailBountyInformation(
    epochProgress             : EpochProgress,
    dorAPIResponse            : DorAPIResponse,
    lastBillingId             : String,
    oldRetailBountyInformation: RetailBountyInformation
  ): RetailBountyInformation = {
    if (oldRetailBountyInformation.lastBillingId == lastBillingId || oldRetailBountyInformation.nextEpochProgressToRewardRetail > epochProgress.value.value) {
      oldRetailBountyInformation
    } else {
      val currentEpochProgress = epochProgress.value.value
      val currentEpochModulus: Long = currentEpochProgress % EpochProgress1Day
      val nextRewardEpoch: Long = currentEpochProgress - currentEpochModulus + EpochProgress1Month + OffsetRetailBounty

      RetailBountyInformation(
        nextRewardEpoch,
        dorAPIResponse.teamId.get,
        dorAPIResponse.lastBillingId.get,
        dorAPIResponse.billedAmountMonthly.get
      )
    }
  }
}