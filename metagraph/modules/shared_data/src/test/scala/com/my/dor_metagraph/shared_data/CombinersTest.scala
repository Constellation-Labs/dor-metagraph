package com.my.dor_metagraph.shared_data

//import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
//import com.my.dor_metagraph.shared_data.Combiners.{combine, getCheckInHash}
//import com.my.dor_metagraph.shared_data.Data.{CheckInRef, DeviceCheckInFormatted, DeviceCheckInInfo, DeviceCheckInRawUpdate, DeviceInfo, FootTraffic, State}
//import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
//import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
//  pureTest("Create a new check in on state") {
//    val oldState = State(Map.empty)
//    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
//    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))
//    val publicKey = "22f97a140b17556e582efa667489bc0097eae9349707790630b0d5133d3b01db67ce0e6adb65e154096036ce5619b5fbdd116dbb07c12f9c68c2306f9830dbc6"
//    val snapshotOrdinal = 10L
//    val checkInHash = "e12345"
//    val epochProgress = 1440L
//    val deviceInfoAPIResponse = DeviceInfoAPIResponse(address, linkedToStore = true, Some("Retail"))
//
//    val allCheckIns = combine(oldState, address, checkInRaw, publicKey, snapshotOrdinal, checkInHash, epochProgress, deviceInfoAPIResponse)
//
//    allCheckIns.devices.get(address) match {
//      case Some(checkIn) =>
//        expect.eql(publicKey, checkIn.publicKey) &&
//          expect.eql(epochProgress, checkIn.lastCheckInEpochProgress) &&
//          expect.eql(checkInHash, checkIn.lastCheckIn.checkInRef.hash) &&
//          expect.eql(snapshotOrdinal, checkIn.lastCheckIn.checkInRef.ordinal) &&
//          expect.eql(checkInRaw.dts, checkIn.lastCheckIn.dts) &&
//          expect.eql(checkInRaw.ac, checkIn.lastCheckIn.ac) &&
//          expect.eql(checkInRaw.e.head.head, checkIn.lastCheckIn.footTraffics.head.timestamp) &&
//          expect.eql(checkInRaw.e.head.last, checkIn.lastCheckIn.footTraffics.head.direction) &&
//          expect.eql(checkInRaw.e.last.head, checkIn.lastCheckIn.footTraffics.last.timestamp) &&
//          expect.eql(checkInRaw.e.last.last, checkIn.lastCheckIn.footTraffics.last.direction)
//      case None =>
//        //forcing failure
//        expect.eql(1, 2)
//    }
//  }
//
//  pureTest("Update check in of device") {
//    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
//    val currentPublicKey = "22f97a140b17556e582efa667489bc0097eae9349707790630b0d5133d3b01db67ce0e6adb65e154096036ce5619b5fbdd116dbb07c12f9c68c2306f9830dbc6"
//    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
//    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
//    val currentEpochProgress = 1440L
//    val currentSnapshotOrdinal = 10L
//    val currentCheckInHash = "e12345"
//    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)), CheckInRef(currentSnapshotOrdinal, currentCheckInHash))
//
//    val oldState = State(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentPublicKey, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))
//
//    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))
//    val allCheckIns = combine(oldState, currentAddress, checkInRaw, currentPublicKey, 20L, "e123410", 2880L, currentDeviceInfoAPIResponse)
//
//    allCheckIns.devices.get(currentAddress) match {
//      case Some(checkIn) =>
//        expect.eql(currentPublicKey, checkIn.publicKey) &&
//          expect.eql(2880L, checkIn.lastCheckInEpochProgress) &&
//          expect.eql("e123410", checkIn.lastCheckIn.checkInRef.hash) &&
//          expect.eql(20L, checkIn.lastCheckIn.checkInRef.ordinal) &&
//          expect.eql(checkInRaw.dts, checkIn.lastCheckIn.dts) &&
//          expect.eql(checkInRaw.ac, checkIn.lastCheckIn.ac) &&
//          expect.eql(checkInRaw.e.head.head, checkIn.lastCheckIn.footTraffics.head.timestamp) &&
//          expect.eql(checkInRaw.e.head.last, checkIn.lastCheckIn.footTraffics.head.direction) &&
//          expect.eql(checkInRaw.e.last.head, checkIn.lastCheckIn.footTraffics.last.timestamp) &&
//          expect.eql(checkInRaw.e.last.last, checkIn.lastCheckIn.footTraffics.last.direction)
//      case None =>
//        //forcing failure
//        expect.eql(1, 2)
//    }
//  }
}