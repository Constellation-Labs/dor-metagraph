package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.types.Types._
import com.my.dor_metagraph.shared_data.validations.TypeValidators.{validateCheckInLimitTimestamp, validateCheckInTimestampIsGreaterThanLastCheckIn, validateIfDeviceIsRegisteredOnDORApi}
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object TypeValidatorsTest extends SimpleIOSuite {

  pureTest("Return update valid - validateCheckInTimestampIsGreaterThanLastCheckIn") {
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty, List.empty, List.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val deviceInfoAPIResponse = DorAPIResponse(address, isInstalled = true, Some("Retail"), Some(10L))
    val checkInRaw = CheckInUpdate("123", "456", 123456, cborString, Some(deviceInfoAPIResponse))

    val validation = validateCheckInTimestampIsGreaterThanLastCheckIn(oldState.calculated, checkInRaw, address)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - validateCheckInTimestampIsGreaterThanLastCheckIn") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L
    val checkInStateOnChain: CheckInStateOnChain = CheckInStateOnChain(List.empty)
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInDataCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, deviceInfoAPIResponse, currentEpochProgress)), List.empty, List.empty)
    val oldState = DataState(checkInStateOnChain, checkInDataCalculatedState)

    val checkInRaw = CheckInUpdate("123", "456", 1, cborString, Some(deviceInfoAPIResponse))
    val validation = validateCheckInTimestampIsGreaterThanLastCheckIn(oldState.calculated, checkInRaw, currentAddress)

    expect.eql(false, validation.isValid)
  }

  pureTest("Return update valid - validateCheckInLimitTimestamp") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInRaw = CheckInUpdate("123", "456", 1693526401L, cborString, Some(deviceInfoAPIResponse))

    val validation = validateCheckInLimitTimestamp(checkInRaw)

    expect.eql(true, validation.isValid)
  }

  pureTest("Return update invalid - validateCheckInLimitTimestamp") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(currentAddress, isInstalled = true, Some("Retail"), Some(10L))
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInRaw = CheckInUpdate("123", "456", 71, cborString, Some(deviceInfoAPIResponse))

    val validation = validateCheckInLimitTimestamp(checkInRaw)

    expect.eql(false, validation.isValid)
  }

  pureTest("Return update invalid - validateIfDeviceIsRegisteredOnDORApi") {
    val cborString = "BF6261639F188F38B43925B8FF636474731A63875B2461659F9F1B00000184A0C9AF5E01FF9F1B00000194A0CD649601FF9F1B00000184A0CE08A701FF9F1B00000184A0D0CF9801FF9F1B00000184A0D3254101FF9F1B00000184A0D3968A01FF9F1B00000184A0D3C95301FF9F1B00000184A0D3F06401FF9F1B00000184A0D47D0501FF9F1B00000184A0D48CA601FFFFFF"
    val checkInRaw = CheckInUpdate("123", "456", 71, cborString, None)

    val validation = validateIfDeviceIsRegisteredOnDORApi(checkInRaw)

    expect.eql(false, validation.isValid)
  }
}