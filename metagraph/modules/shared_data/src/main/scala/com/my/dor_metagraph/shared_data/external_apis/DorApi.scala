package com.my.dor_metagraph.shared_data.external_apis

import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInWithSignature, DorAPIResponse}
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import ujson.Obj

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
        "signature" -> deviceCheckIn.sig
      ).render()

      logger.info(s"Request body: $requestBody")

      val response = requests.post(
        url = s"https://api.getdor.com/metagraph/device/$publicKey/check-in",
        headers = Map("Content-Type" -> "application/json", "version" -> "2"),
        data = requestBody
      )

      val body = response.text()
      logger.info(s"API response $body")

      decode[DorAPIResponse](body) match {
        case Left(err) =>
          logger.warn(s"Error when decoding ${err.getMessage}")
          None
        case Right(deviceInfo) => Some(deviceInfo)
      }
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching API: ${x.getMessage}")
        None
    }
  }
}
