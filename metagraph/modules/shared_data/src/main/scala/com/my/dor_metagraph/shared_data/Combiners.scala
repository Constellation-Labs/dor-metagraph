package com.my.dor_metagraph.shared_data

import Types.{DeviceCheckInFormatted, DeviceCheckInWithSignature, DeviceInfo, FootTraffic, CheckInState}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.{DeviceInfoAPIResponse, fetchDeviceInfo}
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, epochProgress: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val state = acc.devices.get(address)
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)
    val footTraffics = checkInInfo.e.map { event => FootTraffic(event.head, event.last) }
    val checkInFormatted = DeviceCheckInFormatted(checkInInfo.ac, checkInInfo.dts, footTraffics)

    val bounties = state match {
      case Some(current) => current.bounties
      case None => List(UnitDeployedBounty("UnitDeployed"), CommercialLocationBounty("CommercialLocation"))
    }

    val checkIn = DeviceInfo(checkInFormatted, bounties, deviceInfo, epochProgress)
    acc.focus(_.devices).modify(_.updated(address, checkIn))
  }

  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, epochProgress: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val deviceCheckIn = signedUpdate.value

    getNewCheckIn(acc, address, deviceCheckIn, epochProgress, deviceInfo)
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], epochProgress: Long, address: Address): CheckInState = {
    try {
      val publicKey = signedUpdate.proofs.head.id.hex.value
      fetchDeviceInfo(publicKey) match {
        case Some(deviceInfo) => combine(signedUpdate, acc, address, epochProgress, deviceInfo)
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
