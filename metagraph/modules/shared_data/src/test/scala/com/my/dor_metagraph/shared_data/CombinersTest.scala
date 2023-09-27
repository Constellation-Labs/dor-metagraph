package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Combiners.getNewCheckIn
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInWithSignature, DeviceInfo, DeviceInfoAPIResponse, DeviceInfoAPIResponseWithHash}
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
  pureTest("Create a new check in on state") {
    val oldState = CheckInState(List.empty, Map.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInRaw = DeviceCheckInWithSignature(cborString, "", "")
    val epochProgress = 1440L
    val deviceInfoAPIResponse = DeviceInfoAPIResponseWithHash(address, isInstalled = true, Some("Retail"), Some(10L), "123")
    val allCheckIns = getNewCheckIn(oldState, address, checkInRaw, epochProgress, deviceInfoAPIResponse)
    val deviceInfo = allCheckIns.devices(address)

    allCheckIns.updates.find(_.deviceId == address) match {
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
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val currentDeviceInfoAPIResponseWithHash = DeviceInfoAPIResponseWithHash(currentAddress, isInstalled = true, Some("Retail"), Some(10L), "123")
    var currentEpochProgress = 1440L

    val oldState = CheckInState(List.empty, Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val deviceInfo = oldState.devices(currentAddress)

    oldState.updates.find(_.deviceId == currentAddress) match {
      case Some(_) =>
        expect.eql(currentEpochProgress, deviceInfo.nextEpochProgressToReward)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }

    currentEpochProgress = 2882L
    val checkInRaw = DeviceCheckInWithSignature(cborString, "", "")
    val allCheckIns = getNewCheckIn(oldState, currentAddress, checkInRaw, currentEpochProgress, currentDeviceInfoAPIResponseWithHash)

    val deviceInfo2 = allCheckIns.devices(currentAddress)

    allCheckIns.updates.find(_.deviceId == currentAddress) match {
      case Some(checkIn) =>
        expect.eql(4320L, deviceInfo2.nextEpochProgressToReward) &&
          expect.eql("123", checkIn.checkInHash)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }
}