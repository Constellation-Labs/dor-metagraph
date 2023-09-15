package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.DeviceInfoAPIResponse
import io.circe.parser.decode
import org.slf4j.LoggerFactory

object DorApi {
  private val dorApi = DorApi()
  def fetchDeviceInfo(publicKey: String): Option[DeviceInfoAPIResponse] = {
    dorApi.fetchDeviceInfo(publicKey)
  }
}
case class DorApi() {
  private val logger = LoggerFactory.getLogger(classOf[DorApi])
  def fetchDeviceInfo(publicKey: String): Option[DeviceInfoAPIResponse] = {
    try {
      val response = requests.get(s"https://api.getdor.com/metagraph/device-metadata/$publicKey")
      val body = response.text()
      logger.info(s"API response $body")

      decode[DeviceInfoAPIResponse](body) match {
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
