package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.Combiners.getDeviceCheckInFromCBOR
import com.my.dor_metagraph.shared_data.Data.DeviceCheckInRaw
import weaver.SimpleIOSuite
object CombinersTest extends SimpleIOSuite {

  pureTest("Get correctly device check in from CBOR ") {
    val cborString = "A362616383188F38B43925B8636474731A63875B2461658A821B00000184A0C9AF5E01821B00000194A0CD649601821B00000184A0CE04BF01821B00000184A0D0CF9801821B00000184A0D3254101821B00000184A0D3968A01821B00000184A0D3C95301821B00000184A0D3F06401821B00000184A0D47D0501821B00000184A0D48CA601"
    val checkIn = getDeviceCheckInFromCBOR(cborString)

    expect.eql(3, checkIn.ac.size) &&
      expect.eql(143, checkIn.ac.head ) &&
      expect.eql(-181, checkIn.ac(1) ) &&
      expect.eql(-9657, checkIn.ac.last) &&
      expect.eql(1669815076, checkIn.dts) &&
      expect.eql(10, checkIn.e.size) &&
      forEach(checkIn.e)(event => expect.eql(2, event.size))
  }

  test("Fail with invalid body") {
    val cborString = "A36261638318aa8F38B43925B8636474731A63875B2461658A821B00000184A0C9AF5E01821B00000194A0CD649601821B00000184A0CE04BF01821B00000184A0D0CF9801821B00000184A0D3254101821B00000184A0D3968A01821B00000184A0D3C95301821B00000184A0D3F06401821B00000184A0D47D0501821B00000184A0D48CA601"
    for {
      error <- IO[DeviceCheckInRaw](getDeviceCheckInFromCBOR(cborString)).attempt
    } yield {
      val errorMessage = error.left.map(_.getMessage())
      expect(errorMessage == Left("Expected Long but got Array Header (15) (input position 7)"))
    }
  }
}