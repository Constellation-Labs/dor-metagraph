package com.my.dor_metagraph.shared_data.external_apis

import cats.implicits.catsSyntaxOptionId
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInWithSignature, DorAPIResponse}
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import ujson.Obj
import eu.timepit.refined.auto._

object DorApi {
  private val logger = LoggerFactory.getLogger("DorAPI")

  def saveDeviceCheckIn(publicKey: String, deviceCheckIn: DeviceCheckInWithSignature): Option[DorAPIResponse] = {
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)

    logger.info(s"Decoded CBOR field before check-in to DOR Server AC ${checkInInfo.ac}")
    logger.info(s"Decoded CBOR field before check-in to DOR Server DTS ${checkInInfo.dts}")
    logger.info(s"Decoded CBOR field before check-in to DOR Server E ${checkInInfo.e}")

    try {
      val requestBody = Obj(
        "ac" -> checkInInfo.ac,
        "dts" -> checkInInfo.dts,
        "e" -> checkInInfo.e,
        "hash" -> deviceCheckIn.hash,
        "signature" -> deviceCheckIn.sig
      ).render()

      logger.info(s"Request body: $requestBody")

      DorAPIResponse(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, true, None, None).some
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching DOR API: ${x.getMessage}")
        None
    }
  }
}
