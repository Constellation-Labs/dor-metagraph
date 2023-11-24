package com.my.dor_metagraph.shared_data.combiners

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInProof, CheckInStateOnChain, CheckInStateUpdate, CheckInUpdate, DeviceInfo, EPOCH_PROGRESS_1_DAY}
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
    val state = acc.calculated.devices.get(address)

    val currentEpoch: Long = epochProgress.value.value
    val currentEpochModulus: Long = currentEpoch % EPOCH_PROGRESS_1_DAY
    val nextRewardEpoch: Long = currentEpoch - currentEpochModulus + EPOCH_PROGRESS_1_DAY

    val nextRewardEpochProgress: Long = state
      .filter(currentEpoch < _.nextEpochProgressToReward)
      .map(_.nextEpochProgressToReward)
      .getOrElse(nextRewardEpoch)

    val checkInProof = CheckInProof(checkInUpdate.publicId, checkInUpdate.signature)
    val checkInStateUpdate = CheckInStateUpdate(address, checkInUpdate.dts, checkInProof, checkInUpdate.dtmCheckInHash)
    val checkIn = DeviceInfo(checkInUpdate.dts, checkInUpdate.maybeDorAPIResponse.get, nextRewardEpochProgress)

    val devices: Map[Address, DeviceInfo] = acc.calculated.devices.updated(address, checkIn)
    val updates: List[CheckInStateUpdate] = checkInStateUpdate :: acc.onChain.updates

    DataState(
      CheckInStateOnChain(updates),
      CheckInDataCalculatedState(devices)
    )
  }
}