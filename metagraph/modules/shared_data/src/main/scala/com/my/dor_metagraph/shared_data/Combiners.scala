package com.my.dor_metagraph.shared_data

import Types.{CheckInState, DeviceCheckInFormatted, DeviceCheckInWithSignature, DeviceInfo, EPOCH_PROGRESS_1_DAY, FootTraffic}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.{DeviceInfoAPIResponse, fetchDeviceInfo}
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, currentEpoch: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val state = acc.devices.get(address)
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)

    println(s"Decoded CBOR field before combining AC ${checkInInfo.ac}")
    println(s"Decoded CBOR field before combining DTS ${checkInInfo.dts}")
    println(s"Decoded CBOR field before combining E ${checkInInfo.e}")

    val footTraffics = checkInInfo.e.map { event => FootTraffic(event.head, event.last) }
    val checkInFormatted = DeviceCheckInFormatted(checkInInfo.ac, checkInInfo.dts, footTraffics)

    val bounties = state match {
      case Some(current) => current.bounties
      case None => List(UnitDeployedBounty("UnitDeployed"), CommercialLocationBounty("CommercialLocation"))
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

    val checkIn = DeviceInfo(checkInFormatted, bounties, deviceInfo, nextRewardEpochProgress)
    println(s"New checkIn for the device: $checkIn")
    acc.focus(_.devices).modify(_.updated(address, checkIn))
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
        println(e.getMessage)
        println("Ignoring update and keeping with the current state")
        acc
    }
  }
}
