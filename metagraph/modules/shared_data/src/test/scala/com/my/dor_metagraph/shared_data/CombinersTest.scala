package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.getNewCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
  pureTest("Create a new check in on state") {
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address, isInstalled = true, Some("Retail"), Some(10L))
    val checkInRaw = CheckInUpdate("123", "456", 1669815076L, "123", Some(deviceInfoAPIResponse))

    val epochProgress = 1440L
    val allCheckIns = getNewCheckIn(oldState, address, checkInRaw, epochProgress)
    val deviceInfo = allCheckIns.calculated.devices(address)

    allCheckIns.onChain.updates.find(_.deviceId == address) match {
      case Some(checkIn) =>
        expect.eql(epochProgress, deviceInfo.nextEpochProgressToReward)
        expect.eql(1669815076L, checkIn.dts) &&
          expect.eql("123", checkIn.checkInHash)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }

  pureTest("Update check in of device") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    var currentEpochProgress = 1440L

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)

    val deviceInfo = oldState.calculated.devices(currentAddress)

    oldState.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(_) =>
        expect.eql(currentEpochProgress, deviceInfo.nextEpochProgressToReward)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }

    currentEpochProgress = 2882L
    val checkInRaw = CheckInUpdate("123", "456", 12345, "123", Some(currentDeviceInfoAPIResponse))
    val allCheckIns = getNewCheckIn(oldState, currentAddress, checkInRaw, currentEpochProgress)

    val deviceInfo2 = allCheckIns.calculated.devices(currentAddress)

    allCheckIns.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(checkIn) =>
        expect.eql(4320L, deviceInfo2.nextEpochProgressToReward) &&
          expect.eql("123", checkIn.checkInHash)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }
}