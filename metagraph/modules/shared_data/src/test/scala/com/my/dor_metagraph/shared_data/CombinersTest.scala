package com.my.dor_metagraph.shared_data

import cats.syntax.option._
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import org.tessellation.schema.epoch.EpochProgress
import eu.timepit.refined.auto._
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
  pureTest("Create a new check in on state") {
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, 10L.some)
    val checkInRaw = CheckInUpdate("123", "456", 1669815076L, "123", deviceInfoAPIResponse.some)

    val epochProgress = EpochProgress(1440L)
    val allCheckIns = combineDeviceCheckIn(oldState, checkInRaw, address, epochProgress)
    val deviceInfo = allCheckIns.calculated.devices(address)

    allCheckIns.onChain.updates.find(_.deviceId == address) match {
      case Some(checkIn) =>
        expect.eql(epochProgress.value.value, deviceInfo.nextEpochProgressToReward)
        expect.eql(1669815076L, checkIn.dts) &&
          expect.eql("123", checkIn.checkInHash)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }

  pureTest("Update check in of device") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, 10L.some)
    var currentEpochProgress = EpochProgress(1440L)

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress.value.value)))
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)

    val deviceInfo = oldState.calculated.devices(currentAddress)

    oldState.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(_) =>
        expect.eql(currentEpochProgress.value.value, deviceInfo.nextEpochProgressToReward)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }

    currentEpochProgress = EpochProgress(2882L)
    val checkInRaw = CheckInUpdate("123", "456", 12345, "123", currentDeviceInfoAPIResponse.some)
    val allCheckIns = combineDeviceCheckIn(oldState, checkInRaw, currentAddress, currentEpochProgress)

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