package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInFormatted, DeviceCheckInInfo, DeviceInfo, FootTraffic}
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object TypeValidatorsTest extends SimpleIOSuite {

  pureTest("Return update valid - Check timestamp") {
    val oldState = CheckInState(Map.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))

    val validation = validateCheckInTimestamp(oldState, checkInRaw, address)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - Check timestamp") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1440L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))
    val oldState = CheckInState(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))

    val checkInRaw = DeviceCheckInInfo(List(1, 2, 3), 1, List(List(12345, 1), List(6789, -1)))
    val validation = validateCheckInTimestamp(oldState, checkInRaw, currentAddress)

    expect.eql(false, validation.isValid)
  }
}