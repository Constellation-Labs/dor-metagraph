package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.{DeviceCheckInInfoWithSignature, DeviceCheckInWithSignature, DeviceInfoAPIResponseWithHash}
import com.my.dor_metagraph.shared_data.Utils.getDeviceCheckInInfo
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import upickle.default._

object DorApi {
  private val dorApi = DorApi()
  def saveDeviceCheckIn(publicKey: String, deviceCheckIn: DeviceCheckInWithSignature): Option[DeviceInfoAPIResponseWithHash] = {
    dorApi.saveDeviceCheckIn(publicKey, deviceCheckIn)
  }
}
case class DorApi() {
  private val logger = LoggerFactory.getLogger(classOf[DorApi])
  def saveDeviceCheckIn(publicKey: String, deviceCheckIn: DeviceCheckInWithSignature): Option[DeviceInfoAPIResponseWithHash] = {
    val checkInInfo = getDeviceCheckInInfo(deviceCheckIn.cbor)

    implicit val myDataWriter: Writer[DeviceCheckInInfoWithSignature] = macroRW
    val checkInInfoWithSignature = DeviceCheckInInfoWithSignature(checkInInfo.ac, checkInInfo.dts, checkInInfo.e, deviceCheckIn.sig)

    logger.info(s"Decoded CBOR field before check-in to DOR Server AC ${checkInInfo.ac}")
    logger.info(s"Decoded CBOR field before check-in to DOR Server DTS ${checkInInfo.dts}")
    logger.info(s"Decoded CBOR field before check-in to DOR Server E ${checkInInfo.e}")

    try {
      val response = requests.post(s"https://api.getdor.com/metagraph/device/$publicKey/check-in", data = write(checkInInfoWithSignature))

      val body = response.text()
      logger.info(s"API response $body")

      decode[DeviceInfoAPIResponseWithHash](body) match {
        case Left(_) => None
        case Right(deviceInfo) => Some(deviceInfo)
      }
    } catch {
      case x: Exception =>
        logger.error(s"Error when fetching API: ${x.getMessage}")
        None
    }
  }
}
