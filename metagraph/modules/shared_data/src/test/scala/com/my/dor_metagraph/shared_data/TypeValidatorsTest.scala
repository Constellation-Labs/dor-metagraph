package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInInfo, DeviceInfo, DeviceInfoAPIResponse}
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object TypeValidatorsTest extends SimpleIOSuite {

  pureTest("Return update valid - Check timestamp") {
    val oldState = CheckInState(List.empty, Map.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))

    val validation = validateCheckInTimestamp(oldState, checkInRaw, address)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - Check timestamp") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L
    val oldState = CheckInState(List.empty, Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress)))

    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 1, List(List(12345, 1), List(6789, -1)))
    val validation = validateCheckInTimestamp(oldState, checkInRaw, currentAddress)

    expect.eql(false, validation.isValid)
  }
}