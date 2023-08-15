package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Combiners.{combine, getCheckInHash, getDeviceCheckInFromCBOR}
import com.my.dor_metagraph.shared_data.Data.{CheckInRef, DeviceCheckInFormatted, DeviceCheckInRawUpdate, DeviceCheckInWithSignature, DeviceInfo, FootTraffic, State}
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object CombinersTest extends SimpleIOSuite {

  pureTest("Get correctly device check in from CBOR ") {
    val cborString = "A562616383188F38B43925B8636474731A63875B2461658A821B00000184A0C9AF5E01821B00000194A0CD649601821B00000184A0CE08A701821B00000184A0D0CF9801821B00000184A0D3254101821B00000184A0D3968A01821B00000184A0D3C95301821B00000184A0D3F06401821B00000184A0D47D0501821B00000184A0D48CA60162696478803664333832383661363632306436373534343864653833363861616438646165656538373539306336666661356466363139656535323562323734373237623662656539323235376166373133363935363330346630643437356339623034326137353835393137353435643132356434396261666338386435336662636365697369676E6174757265788E33303435303232313030646463623366353935633565306534323534346138376534303661326439303136653430333031623061376133656530656664326235333562306537633230323032323034386231633464666236623132396238343064346139646231666233383564366631323164303435626262616363393261616363333038373735623636313133"
    val checkIn = getDeviceCheckInFromCBOR(cborString)

    expect.eql(3, checkIn.ac.size) &&
      expect.eql(143, checkIn.ac.head) &&
      expect.eql(-181, checkIn.ac(1)) &&
      expect.eql(-9657, checkIn.ac.last) &&
      expect.eql(1669815076, checkIn.dts) &&
      expect.eql(10, checkIn.e.size) &&
      forEach(checkIn.e)(event => expect.eql(2, event.size))
  }

  test("Fail with invalid body") {
    val cborString = "A36261638318aa8F38B43925B8636474731A63875B2461658A821B00000184A0C9AF5E01821B00000194A0CD649601821B00000184A0CE04BF01821B00000184A0D0CF9801821B00000184A0D3254101821B00000184A0D3968A01821B00000184A0D3C95301821B00000184A0D3F06401821B00000184A0D47D0501821B00000184A0D48CA601"
    for {
      error <- IO[DeviceCheckInWithSignature](getDeviceCheckInFromCBOR(cborString)).attempt
    } yield {
      val errorMessage = error.left.map(_.getMessage())
      expect(errorMessage == Left("Expected Long but got Array Header (15) (input position 7)"))
    }
  }

  pureTest("Get check in hash successfully") {
    val checkIn = getCheckInHash(DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1))))

    expect.eql("f8c0e15b38981a882790e7d4f70eb7ea2ea28309769cf24af61f5f1dd29ad597", checkIn)
  }

  pureTest("Create a new check in on state") {
    val oldState = State(Map.empty)
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))
    val publicKey = "22f97a140b17556e582efa667489bc0097eae9349707790630b0d5133d3b01db67ce0e6adb65e154096036ce5619b5fbdd116dbb07c12f9c68c2306f9830dbc6"
    val snapshotOrdinal = 10L
    val checkInHash = "e12345"
    val epochProgress = 1440L
    val deviceInfoAPIResponse = DeviceInfoAPIResponse(address, linkedToStore = true, Some("Retail"))

    val allCheckIns = combine(oldState, address, checkInRaw, publicKey, snapshotOrdinal, checkInHash, epochProgress, deviceInfoAPIResponse)

    allCheckIns.devices.get(address) match {
      case Some(checkIn) =>
        expect.eql(publicKey, checkIn.publicKey) &&
          expect.eql(epochProgress, checkIn.lastCheckInEpochProgress) &&
          expect.eql(checkInHash, checkIn.lastCheckIn.checkInRef.hash) &&
          expect.eql(snapshotOrdinal, checkIn.lastCheckIn.checkInRef.ordinal) &&
          expect.eql(checkInRaw.dts, checkIn.lastCheckIn.dts) &&
          expect.eql(checkInRaw.ac, checkIn.lastCheckIn.ac) &&
          expect.eql(checkInRaw.e.head.head, checkIn.lastCheckIn.footTraffics.head.timestamp) &&
          expect.eql(checkInRaw.e.head.last, checkIn.lastCheckIn.footTraffics.head.direction) &&
          expect.eql(checkInRaw.e.last.head, checkIn.lastCheckIn.footTraffics.last.timestamp) &&
          expect.eql(checkInRaw.e.last.last, checkIn.lastCheckIn.footTraffics.last.direction)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }

  pureTest("Update check in of device") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentPublicKey = "22f97a140b17556e582efa667489bc0097eae9349707790630b0d5133d3b01db67ce0e6adb65e154096036ce5619b5fbdd116dbb07c12f9c68c2306f9830dbc6"
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1440L
    val currentSnapshotOrdinal = 10L
    val currentCheckInHash = "e12345"
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)), CheckInRef(currentSnapshotOrdinal, currentCheckInHash))

    val oldState = State(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentPublicKey, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))

    val checkInRaw = DeviceCheckInRawUpdate(List(1, 2, 3), 123456, List(List(12345, 1), List(6789, -1)))
    val allCheckIns = combine(oldState, currentAddress, checkInRaw, currentPublicKey, 20L, "e123410", 2880L, currentDeviceInfoAPIResponse)

    allCheckIns.devices.get(currentAddress) match {
      case Some(checkIn) =>
        expect.eql(currentPublicKey, checkIn.publicKey) &&
          expect.eql(2880L, checkIn.lastCheckInEpochProgress) &&
          expect.eql("e123410", checkIn.lastCheckIn.checkInRef.hash) &&
          expect.eql(20L, checkIn.lastCheckIn.checkInRef.ordinal) &&
          expect.eql(checkInRaw.dts, checkIn.lastCheckIn.dts) &&
          expect.eql(checkInRaw.ac, checkIn.lastCheckIn.ac) &&
          expect.eql(checkInRaw.e.head.head, checkIn.lastCheckIn.footTraffics.head.timestamp) &&
          expect.eql(checkInRaw.e.head.last, checkIn.lastCheckIn.footTraffics.head.direction) &&
          expect.eql(checkInRaw.e.last.head, checkIn.lastCheckIn.footTraffics.last.timestamp) &&
          expect.eql(checkInRaw.e.last.last, checkIn.lastCheckIn.footTraffics.last.direction)
      case None =>
        //forcing failure
        expect.eql(1, 2)
    }
  }
}