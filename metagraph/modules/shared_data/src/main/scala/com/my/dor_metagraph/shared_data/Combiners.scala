package com.my.dor_metagraph.shared_data

import Types.{CheckInProof, CheckInState, CheckInUpdates, DeviceCheckInFormatted, DeviceCheckInWithSignature, DeviceInfo, DeviceInfoAPIResponse, EPOCH_PROGRESS_1_DAY, FootTraffic}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, RetailAnalyticsSubscriptionBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.fetchDeviceInfo
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import org.slf4j.LoggerFactory
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  private val combiners: Combiners = Combiners()
  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, currentEpoch: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    combiners.getNewCheckIn(acc, address, deviceCheckIn, currentEpoch, deviceInfo)
  }
  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, currentEpochProgress: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    combiners.combine(signedUpdate, acc, address, currentEpochProgress, deviceInfo)
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], currentEpochProgress: Long, address: Address): CheckInState = {
    combiners.combineDeviceCheckIn(acc, signedUpdate, currentEpochProgress, address)
  }
}
case class Combiners() {
  private val logger = LoggerFactory.getLogger(classOf[Combiners])
  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, currentEpoch: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val state = acc.devices.get(address)
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)

   logger.info(s"Decoded CBOR field before combining AC ${checkInInfo.ac}")
   logger.info(s"Decoded CBOR field before combining DTS ${checkInInfo.dts}")
   logger.info(s"Decoded CBOR field before combining E ${checkInInfo.e}")

    val footTraffics = checkInInfo.e.map { event => FootTraffic(event.head, event.last) }
    val checkInFormatted = DeviceCheckInFormatted(checkInInfo.ac, checkInInfo.dts, footTraffics)

    val bounties = state match {
      case Some(current) => current.bounties
      case None => List(UnitDeployedBounty(), CommercialLocationBounty(), RetailAnalyticsSubscriptionBounty())
    }

    val currentEpochModulus = currentEpoch % EPOCH_PROGRESS_1_DAY
    val nextRewardEpoch = currentEpoch - currentEpochModulus + EPOCH_PROGRESS_1_DAY

    val nextRewardEpochProgress = state match {
      case Some(current) =>
        if (currentEpoch >= current.nextEpochProgressToReward ) {
          nextRewardEpoch
        } else {
          current.nextEpochProgressToReward
        }
      case None => nextRewardEpoch
    }

    val checkInProof = CheckInProof(deviceCheckIn.id, deviceCheckIn.sig)
    val checkInUpdate = CheckInUpdates(address, checkInFormatted, checkInProof)

    val checkIn = DeviceInfo(bounties, checkInInfo.dts, deviceInfo, nextRewardEpochProgress)
    logger.info(s"New checkIn for the device: $checkIn")

    val devices = acc.devices.updated(address, checkIn)
    val updates = checkInUpdate :: acc.updates

    CheckInState(updates, devices)
  }

  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, currentEpochProgress: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val deviceCheckIn = signedUpdate.value

    getNewCheckIn(acc, address, deviceCheckIn, currentEpochProgress, deviceInfo)
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], currentEpochProgress: Long, address: Address): CheckInState = {
    try {
      val publicKey = signedUpdate.proofs.head.id.hex.value
      fetchDeviceInfo(publicKey) match {
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
