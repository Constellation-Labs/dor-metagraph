package com.my.dor_metagraph.shared_data

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.parser.decode
import org.tessellation.schema.address.Address

object DorApi {
  @derive(decoder, encoder)
  case class DeviceInfoAPIResponse(rewardAddress: Address, linkedToStore: Boolean, storeType: Option[String])
  def fetchDeviceInfo(publicKey: String): Option[DeviceInfoAPIResponse] = {
    try {
      val response = requests.get(s"http://host.docker.internal:3333/metagraph/devices/${publicKey}")
      val body = response.text()
      println("API response" + body)

      decode[DeviceInfoAPIResponse](body) match {
        case Left(_) => None
        case Right(deviceInfo) => Some(deviceInfo)
      }
    } catch {
      case x: Exception =>
        println(s"Error when fetching API: ${x.getMessage}")
        None
    }
  }
}
