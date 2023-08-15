package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Data.{CheckInRef, DeviceCheckInFormatted, DeviceCheckInRawUpdate, DeviceInfo, FootTraffic, State}
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object TypeValidatorsTest extends SimpleIOSuite {

  pureTest("Return update valid - Check timestamp") {
    val oldState = State(Map.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))

    val validation = validateCheckInTimestamp(oldState, checkInRaw, address)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - Check timestamp") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentPublicKey = "22f97a140b17556e582efa667489bc0097eae9349707790630b0d5133d3b01db67ce0e6adb65e154096036ce5619b5fbdd116dbb07c12f9c68c2306f9830dbc6"
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1440L
    val currentSnapshotOrdinal = 10L
    val currentCheckInHash = "e12345"
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)), CheckInRef(currentSnapshotOrdinal, currentCheckInHash))
    val oldState = State(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentPublicKey, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))

    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 1, List(List(12345, 1), List(6789, -1)))
    val validation = validateCheckInTimestamp(oldState, checkInRaw, currentAddress)

    expect.eql(false, validation.isValid)
  }
}