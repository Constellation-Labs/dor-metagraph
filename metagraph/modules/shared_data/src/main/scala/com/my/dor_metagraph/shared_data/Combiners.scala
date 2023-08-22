package com.my.dor_metagraph.shared_data

import Types.{DeviceCheckInFormatted, DeviceCheckInTransaction, DeviceCheckInWithSignature, DeviceInfo, FootTraffic, LastSnapshotRefs, LastTxnRefs, CheckInState}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.{DeviceInfoAPIResponse, fetchDeviceInfo}
import com.my.dor_metagraph.shared_data.Utils.{getCheckInHash, getDeviceCheckInInfo}
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  private def getNewCheckIn(acc: CheckInState, address: Address, deviceCheckIn: DeviceCheckInWithSignature, publicKey: String, epochProgress: Long, deviceInfo: DeviceInfoAPIResponse) = {
    val state = acc.devices.get(address)
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)
    val footTraffics = checkInInfo.e.map { event => FootTraffic(event.head, event.last) }
    val checkInFormatted = DeviceCheckInFormatted(checkInInfo.ac, checkInInfo.dts, footTraffics)

    val bounties = state match {
      case Some(current) => current.bounties
      case None => List(UnitDeployedBounty("UnitDeployed"), CommercialLocationBounty("CommercialLocation"))
    }

    DeviceInfo(checkInFormatted, publicKey, bounties, deviceInfo, epochProgress)
  }

  def combine(signedUpdate: Signed[DeviceCheckInWithSignature], acc: CheckInState, address: Address, currentSnapshotOrdinal: Long, epochProgress: Long, deviceInfo: DeviceInfoAPIResponse): CheckInState = {
    val publicKey = signedUpdate.proofs.head.id.hex.value
    val deviceCheckIn = signedUpdate.value
    val checkInHash = getCheckInHash(signedUpdate.value)

    val checkIn = getNewCheckIn(acc, address, deviceCheckIn, publicKey, epochProgress, deviceInfo)

    val lastTxnRefs = acc.lastTxnRefs.getOrElse(address, LastTxnRefs.empty)
    val lastSnapshotRefs = acc.lastSnapshotRefs.getOrElse(address, null)

    val updateUsageTransaction = DeviceCheckInTransaction(address, currentSnapshotOrdinal)
    val newLastTxnRef = LastTxnRefs(currentSnapshotOrdinal, lastTxnRefs.txnOrdinal + 1, checkInHash)

    if (lastSnapshotRefs == null) {
      acc.focus(_.devices).modify(_.updated(address, checkIn))
        .focus(_.transactions).modify(_.updated(checkInHash, updateUsageTransaction))
        .focus(_.lastTxnRefs).modify(_.updated(address, newLastTxnRef))
        .focus(_.lastSnapshotRefs).modify(_.updated(address, LastSnapshotRefs.empty))
    } else if (lastTxnRefs.snapshotOrdinal == currentSnapshotOrdinal) {
      acc.focus(_.devices).modify(_.updated(address, checkIn))
        .focus(_.transactions).modify(_.updated(checkInHash, updateUsageTransaction))
        .focus(_.lastTxnRefs).modify(_.updated(address, newLastTxnRef))
        .focus(_.lastSnapshotRefs).modify(_.updated(address, lastSnapshotRefs))
    } else {
      val createdLastRef = LastSnapshotRefs(lastTxnRefs.snapshotOrdinal, lastTxnRefs.hash)
      acc.focus(_.devices).modify(_.updated(address, checkIn))
        .focus(_.transactions).modify(_.updated(checkInHash, updateUsageTransaction))
        .focus(_.lastTxnRefs).modify(_.updated(address, newLastTxnRef))
        .focus(_.lastSnapshotRefs).modify(_.updated(address, createdLastRef))
    }
  }

  def combineDeviceCheckIn(acc: CheckInState, signedUpdate: Signed[DeviceCheckInWithSignature], ordinal: Long, epochProgress: Long, address: Address): CheckInState = {
    try {
      val publicKey = signedUpdate.proofs.head.id.hex.value
      fetchDeviceInfo(publicKey) match {
        case Some(deviceInfo) => combine(signedUpdate, acc, address, ordinal, epochProgress, deviceInfo)
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
