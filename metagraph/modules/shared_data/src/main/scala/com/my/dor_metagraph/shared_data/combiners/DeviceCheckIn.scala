package com.my.dor_metagraph.shared_data.combiners

import cats.syntax.option._
import com.my.dor_metagraph.shared_data.types.Types._
import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress

object DeviceCheckIn {
  def combineDeviceCheckIn(
    acc          : DataState[CheckInStateOnChain, CheckInDataCalculatedState],
    checkInUpdate: CheckInUpdate,
    address      : Address,
    epochProgress: EpochProgress
  ): DataState[CheckInStateOnChain, CheckInDataCalculatedState] =
    // Validation rejects check-ins without a DOR API response before they reach a block, so
    // this branch is only reachable for already-accepted malformed data; skipping the single
    // update keeps the combine total instead of failing the whole snapshot.
    checkInUpdate.maybeDorAPIResponse.fold(acc) { dorAPIResponse =>
      val maybeDeviceInfo = acc.calculated.devices.get(address)

      val nextRewardEpochProgress: Long = nextEpochProgressToReward(epochProgress, maybeDeviceInfo)
      val maybeAnalyticsBountyInformation: Option[AnalyticsBountyInformation] = getAnalyticsRewardsInformation(epochProgress, maybeDeviceInfo, dorAPIResponse)

      val checkInProof = CheckInProof(checkInUpdate.publicId, checkInUpdate.signature)
      val checkInStateUpdate = CheckInStateUpdate(address, checkInUpdate.dts, checkInProof, checkInUpdate.dtmCheckInHash)

      val checkIn = DeviceInfo(checkInUpdate.dts, dorAPIResponse, nextRewardEpochProgress, maybeAnalyticsBountyInformation, checkInUpdate.publicId.some)

      val devices: Map[Address, DeviceInfo] = acc.calculated.devices.updated(address, checkIn)
      val updates: List[CheckInStateUpdate] = checkInStateUpdate :: acc.onChain.updates

      DataState(
        CheckInStateOnChain(updates),
        CheckInDataCalculatedState(devices, acc.calculated.lastEpochProgress)
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
    dorAPIResponse.lastBillingId.flatMap { lastBillingId =>
      maybeDeviceInfo
        .flatMap(_.analyticsBountyInformation)
        .map(old => updateAnalyticsBountyInformation(epochProgress, dorAPIResponse, lastBillingId, old).some)
        .getOrElse(createAnalyticsBountyInformation(epochProgress, dorAPIResponse))
    }

  private def createAnalyticsBountyInformation(
    epochProgress : EpochProgress,
    dorAPIResponse: DorAPIResponse
  ): Option[AnalyticsBountyInformation] = {
    val (_, nextRewardEpoch: Long) = getRewardEpoch(epochProgress)

    for {
      teamId        <- dorAPIResponse.teamId
      lastBillingId <- dorAPIResponse.lastBillingId
      billedAmount  <- dorAPIResponse.billedAmount
    } yield AnalyticsBountyInformation(
      nextRewardEpoch + ModulusAnalyticsBounty,
      teamId,
      lastBillingId,
      billedAmount,
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
      createAnalyticsBountyInformation(epochProgress, dorAPIResponse).getOrElse(oldAnalyticsBountyInformation)
    }
  }
}