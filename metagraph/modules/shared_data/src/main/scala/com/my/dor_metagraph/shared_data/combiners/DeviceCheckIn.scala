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
    val maybeAnalyticsBountyInformation: Option[AnalyticsBountyInformation] = getAnalyticsRewardsInformation(epochProgress, maybeDeviceInfo, dorAPIResponse)

    val checkInProof = CheckInProof(checkInUpdate.publicId, checkInUpdate.signature)
    val checkInStateUpdate = CheckInStateUpdate(address, checkInUpdate.dts, checkInProof, checkInUpdate.dtmCheckInHash)

    val checkIn = DeviceInfo(checkInUpdate.dts, dorAPIResponse, nextRewardEpochProgress, maybeAnalyticsBountyInformation)

    val devices: Map[Address, DeviceInfo] = acc.calculated.devices.updated(address, checkIn)
    val updates: List[CheckInStateUpdate] = checkInStateUpdate :: acc.onChain.updates

    DataState(
      CheckInStateOnChain(updates),
      CheckInDataCalculatedState(devices)
    )
  }

  private def getRewardEpoch(epochProgress: EpochProgress): (Long, Long) = {
    val currentEpoch: Long = epochProgress.value.value
    val currentEpochModulus: Long = currentEpoch % EpochProgress1Day
    val nextRewardEpoch: Long = currentEpoch - currentEpochModulus + EpochProgress1Day
    (currentEpoch, nextRewardEpoch)
  }

  private def nextEpochProgressToReward(
    epochProgress  : EpochProgress,
    maybeDeviceInfo: Option[DeviceInfo]
  ): Long = {
    val (currentEpoch: Long, nextRewardEpoch: Long) = getRewardEpoch(epochProgress)

    maybeDeviceInfo
      .filter(_.nextEpochProgressToReward > currentEpoch)
      .map(_.nextEpochProgressToReward)
      .getOrElse(nextRewardEpoch)
  }

  private def getAnalyticsRewardsInformation(
    epochProgress  : EpochProgress,
    maybeDeviceInfo: Option[DeviceInfo],
    dorAPIResponse : DorAPIResponse
  ): Option[AnalyticsBountyInformation] =
    dorAPIResponse
      .lastBillingId
      .map { lastBillingId =>
        maybeDeviceInfo
          .flatMap(_.analyticsBountyInformation)
          .map(updateAnalyticsBountyInformation(epochProgress, dorAPIResponse, lastBillingId, _))
          .getOrElse(createAnalyticsBountyInformation(epochProgress, dorAPIResponse))
      }

  private def createAnalyticsBountyInformation(
    epochProgress : EpochProgress,
    dorAPIResponse: DorAPIResponse
  ): AnalyticsBountyInformation = {
    val (_, nextRewardEpoch: Long) = getRewardEpoch(epochProgress)

    AnalyticsBountyInformation(
      nextRewardEpoch + ModulusAnalyticsBounty,
      dorAPIResponse.teamId.get,
      dorAPIResponse.lastBillingId.get,
      dorAPIResponse.billedAmount.get,
      dorAPIResponse.orgRewardAddress,
    )
  }

  private def updateAnalyticsBountyInformation(
    epochProgress                : EpochProgress,
    dorAPIResponse               : DorAPIResponse,
    lastBillingId                : Long,
    oldAnalyticsBountyInformation: AnalyticsBountyInformation
  ): AnalyticsBountyInformation = {
    if (oldAnalyticsBountyInformation.lastBillingId == lastBillingId || oldAnalyticsBountyInformation.nextEpochProgressToRewardAnalytics > epochProgress.value.value) {
      oldAnalyticsBountyInformation
    } else {
      createAnalyticsBountyInformation(epochProgress, dorAPIResponse)
    }
  }
}