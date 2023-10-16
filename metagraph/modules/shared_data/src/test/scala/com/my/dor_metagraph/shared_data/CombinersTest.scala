package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.combiners.Combiners.getNewCheckIn
import com.my.dor_metagraph.shared_data.types.Types.{CheckInStateOnChain, DeviceCheckInWithSignature, DeviceInfo, DeviceInfoAPIResponse, DeviceInfoWithCheckInHash}
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
  pureTest("Create a new check in on state") {
    val oldState = CheckInState(List.empty, Map.empty, List.empty, List.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInRaw = DeviceCheckInWithSignature(cborString, "", "")
    val epochProgress = 1440L
    val deviceInfoAPIResponse = DeviceInfoWithCheckInHash(address, isInstalled = true, Some("Retail"), Some(10L), "123")
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
    val cborString = "bf6261639f39260e3901dd190886ff636474731a6516d81661659fff72626174746572795f766f6c746167655f6d760062637468696e74657276616c617200616e64576946696261706957696669547269616c6264746764656661756c7462667765302e302e30781b626f6f746c6f616465725f6669726d776172655f76657273696f6e65302e302e30ff"
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val currentDeviceInfoAPIResponseWithHash = DeviceInfoWithCheckInHash(currentAddress, isInstalled = true, Some("Retail"), Some(10L), "123")
    var currentEpochProgress = 1440L

    val oldState = CheckInState(List.empty, Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)), List.empty, List.empty)
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