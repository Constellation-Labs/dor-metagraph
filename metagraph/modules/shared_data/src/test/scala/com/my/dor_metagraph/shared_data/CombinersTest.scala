package com.my.dor_metagraph.shared_data

import cats.syntax.option._
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.auto._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import org.tessellation.schema.epoch.EpochProgress
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {
  pureTest("Create a new check in on state") {
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, none, none, none, none, none)
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
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    var currentEpochProgress = EpochProgress(1440L)

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(
      Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress.value.value, none)))
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

  pureTest("Create a new check in on state - AnalyticsBountyInformation") {
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    val checkInRaw = CheckInUpdate("123", "456", 1669815076L, "123", deviceInfoAPIResponse.some)

    val epochProgress = EpochProgress(1439L)
    val allCheckIns = combineDeviceCheckIn(oldState, checkInRaw, address, epochProgress)
    val deviceInfo = allCheckIns.calculated.devices(address)

    allCheckIns.onChain.updates.find(_.deviceId == address).fold(throw new Exception("Device not found")) { checkIn =>
      expect.eql(1440L, deviceInfo.nextEpochProgressToReward) &&
        expect.eql(1669815076L, checkIn.dts) &&
        expect.eql("123", checkIn.checkInHash) &&
        expect.eql(1442L, deviceInfo.analyticsBountyInformation.get.nextEpochProgressToRewardAnalytics) &&
        expect.eql(deviceInfoAPIResponse.teamId.get, deviceInfo.analyticsBountyInformation.get.teamId) &&
        expect.eql(deviceInfoAPIResponse.billedAmount.get, deviceInfo.analyticsBountyInformation.get.billedAmount)
    }
  }

  pureTest("Update check in of device - AnalyticsBountyInformation do not update when billingID is the same than last") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    var currentEpochProgress = EpochProgress(1440L)

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(
      Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress.value.value, AnalyticsBountyInformation(1440L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)))
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
          expect.eql("123", checkIn.checkInHash) &&
          expect.eql(1440L, deviceInfo2.analyticsBountyInformation.get.nextEpochProgressToRewardAnalytics) &&
          expect.eql(currentDeviceInfoAPIResponse.teamId.get, deviceInfo2.analyticsBountyInformation.get.teamId) &&
          expect.eql(currentDeviceInfoAPIResponse.billedAmount.get, deviceInfo2.analyticsBountyInformation.get.billedAmount)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }

  pureTest("Update check in of device - AnalyticsBountyInformation update when billingID is different than last") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    var currentEpochProgress = EpochProgress(1440L)

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(
      Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress.value.value, AnalyticsBountyInformation(1440L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)))
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)

    val deviceInfo = oldState.calculated.devices(currentAddress)

    oldState.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(_) =>
        expect.eql(currentEpochProgress.value.value, deviceInfo.nextEpochProgressToReward)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }

    currentEpochProgress = EpochProgress(3000L)
    val checkInRaw = CheckInUpdate("123", "456", 12345, "123", currentDeviceInfoAPIResponse.copy(lastBillingId = 456L.some).some)
    val allCheckIns = combineDeviceCheckIn(oldState, checkInRaw, currentAddress, currentEpochProgress)

    val deviceInfo2 = allCheckIns.calculated.devices(currentAddress)

    allCheckIns.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(checkIn) =>
        expect.eql(4320L, deviceInfo2.nextEpochProgressToReward) &&
          expect.eql("123", checkIn.checkInHash) &&
          expect.eql(4322L, deviceInfo2.analyticsBountyInformation.get.nextEpochProgressToRewardAnalytics) &&
          expect.eql(currentDeviceInfoAPIResponse.teamId.get, deviceInfo2.analyticsBountyInformation.get.teamId) &&
          expect.eql(currentDeviceInfoAPIResponse.billedAmount.get, deviceInfo2.analyticsBountyInformation.get.billedAmount)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }

  pureTest("Update check in of device - AnalyticsBountyInformation do not update when nextEpochProgressToRewardAnalytics is greater than current") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    var currentEpochProgress = EpochProgress(1440L)

    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(
      Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress.value.value, AnalyticsBountyInformation(20000L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)))
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)

    val deviceInfo = oldState.calculated.devices(currentAddress)

    oldState.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(_) =>
        expect.eql(currentEpochProgress.value.value, deviceInfo.nextEpochProgressToReward)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }

    currentEpochProgress = EpochProgress(3000L)
    val checkInRaw = CheckInUpdate("123", "456", 12345, "123", currentDeviceInfoAPIResponse.some)
    val allCheckIns = combineDeviceCheckIn(oldState, checkInRaw, currentAddress, currentEpochProgress)

    val deviceInfo2 = allCheckIns.calculated.devices(currentAddress)

    allCheckIns.onChain.updates.find(_.deviceId == currentAddress) match {
      case Some(checkIn) =>
        expect.eql(4320L, deviceInfo2.nextEpochProgressToReward) &&
          expect.eql("123", checkIn.checkInHash) &&
          expect.eql(20000L, deviceInfo2.analyticsBountyInformation.get.nextEpochProgressToRewardAnalytics) &&
          expect.eql(currentDeviceInfoAPIResponse.teamId.get, deviceInfo2.analyticsBountyInformation.get.teamId) &&
          expect.eql(currentDeviceInfoAPIResponse.billedAmount.get, deviceInfo2.analyticsBountyInformation.get.billedAmount)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }
}