package com.my.dor_metagraph.shared_data

import Types.{CheckInProof, CheckInState, CheckInUpdates, DeviceCheckInWithSignature, DeviceInfo, DeviceInfoAPIResponse, DeviceInfoAPIResponseWithHash, EPOCH_PROGRESS_1_DAY}
import com.my.dor_metagraph.shared_data.DorApi.saveDeviceCheckIn
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  private val combiners: Combiners = Combiners()

  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, currentEpoch: Long, deviceInfo: DeviceInfoAPIResponseWithHash): CheckInState = {
    combiners.getNewCheckIn(acc, address, deviceCheckIn, currentEpoch, deviceInfo)
  }

  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, currentEpochProgress: Long, deviceInfo: DeviceInfoAPIResponseWithHash): CheckInState = {
    combiners.combine(signedUpdate, acc, address, currentEpochProgress, deviceInfo)
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], currentEpochProgress: Long, address: Address): CheckInState = {
    combiners.combineDeviceCheckIn(acc, signedUpdate, currentEpochProgress, address)
  }
}

case class Combiners() {
  private val logger = LoggerFactory.getLogger(classOf[Combiners])

  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, currentEpoch: Long, deviceInfo: DeviceInfoAPIResponseWithHash): CheckInState = {
    val state = acc.devices.get(address)
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)

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

    val checkInProof = CheckInProof(deviceCheckIn.id, deviceCheckIn.sig)
    val checkInUpdate = CheckInUpdates(address, checkInInfo.dts, checkInProof, deviceInfo.hash)

    val deviceInfoApiResponse = DeviceInfoAPIResponse(deviceInfo.rewardAddress, deviceInfo.isInstalled, deviceInfo.locationType, deviceInfo.billedAmountMonthly)
    val checkIn = DeviceInfo(checkInInfo.dts, deviceInfoApiResponse, nextRewardEpochProgress)
    logger.info(s"New checkIn for the device: $checkIn")

    val devices = acc.devices.updated(address, checkIn)
    val updates = checkInUpdate :: acc.updates

    CheckInState(updates, devices)
  }

  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, currentEpochProgress: Long, deviceInfo: DeviceInfoAPIResponseWithHash): CheckInState = {
    val deviceCheckIn = signedUpdate.value

    getNewCheckIn(acc, address, deviceCheckIn, currentEpochProgress, deviceInfo)
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], currentEpochProgress: Long, address: Address): CheckInState = {
    try {
      val publicKey = signedUpdate.proofs.head.id.hex.value
      saveDeviceCheckIn(publicKey, signedUpdate.value) match {
        case Some(deviceInfo) => combine(signedUpdate, acc, address, currentEpochProgress, deviceInfo)
        case None => acc
      }
    } catch {
      case e: Exception =>
        logger.warn(e.getMessage)
        logger.warn("Ignoring update and keeping with the current state")
        acc
    }
  }
}
