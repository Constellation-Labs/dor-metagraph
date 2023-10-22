package com.my.dor_metagraph.shared_data.combiners

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInProof, CheckInStateOnChain, CheckInStateUpdate, CheckInUpdate, DeviceInfo, EPOCH_PROGRESS_1_DAY}
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address

object DeviceCheckIn {
  private val logger = LoggerFactory.getLogger("DeviceCheckIn")

  def getNewCheckIn(acc: DataState[CheckInStateOnChain, CheckInDataCalculatedState], address: Address, checkInUpdate: CheckInUpdate, currentEpoch: Long): DataState[CheckInStateOnChain, CheckInDataCalculatedState] = {
    val state = acc.calculated.devices.get(address)

    val currentEpochModulus = currentEpoch % EPOCH_PROGRESS_1_DAY
    val nextRewardEpoch = currentEpoch - currentEpochModulus + EPOCH_PROGRESS_1_DAY

    val nextRewardEpochProgress = state match {
      case Some(current) =>
        if (currentEpoch >= current.nextEpochProgressToReward) {
          nextRewardEpoch
        } else {
          current.nextEpochProgressToReward
        }
      case None => nextRewardEpoch
    }

    val checkInProof = CheckInProof(checkInUpdate.publicId, checkInUpdate.signature)
    val checkInStateUpdate = CheckInStateUpdate(address, checkInUpdate.dts, checkInProof, checkInUpdate.dtmCheckInHash)

    val checkIn = DeviceInfo(checkInUpdate.dts, checkInUpdate.maybeDorAPIResponse.get, nextRewardEpochProgress)

    logger.info(s"New checkIn for the device: $checkIn")

    val devices = acc.calculated.devices.updated(address, checkIn)
    val updates = checkInStateUpdate :: acc.onChain.updates

    DataState(
      CheckInStateOnChain(updates),
      CheckInDataCalculatedState(devices, acc.calculated.l0ValidatorNodesAddresses, acc.calculated.l1ValidatorNodesAddresses)
    )
  }
}