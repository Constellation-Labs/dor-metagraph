package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInInfo, DeviceInfo, DeviceInfoAPIResponse}
import com.my.dor_metagraph.shared_data.TypeValidators.{validateCheckInTimestampIsGreaterThanLastCheckIn, validateCheckInLimitTimestamp}
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object TypeValidatorsTest extends SimpleIOSuite {

  pureTest("Return update valid - validateCheckInTimestampIsGreaterThanLastCheckIn") {
    val oldState = CheckInState(List.empty, Map.empty, List.empty, List.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))

    val validation = validateCheckInTimestampIsGreaterThanLastCheckIn(oldState, checkInRaw, address)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - validateCheckInTimestampIsGreaterThanLastCheckIn") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L
    val oldState = CheckInState(List.empty, Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)), List.empty, List.empty)

    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 1, List(List(12345, 1), List(6789, -1)))
    val validation = validateCheckInTimestampIsGreaterThanLastCheckIn(oldState, checkInRaw, currentAddress)

    expect.eql(false, validation.isValid)
  }

  pureTest("Return update valid - validateCheckInLimitTimestamp") {
    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 1693526401L, List(List(12345, 1), List(6789, -1)))

    val validation = validateCheckInLimitTimestamp(checkInRaw)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - validateCheckInLimitTimestamp") {
    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 71, List(List(12345, 1), List(6789, -1)))
    val validation = validateCheckInLimitTimestamp(checkInRaw)

    expect.eql(false, validation.isValid)
  }
}