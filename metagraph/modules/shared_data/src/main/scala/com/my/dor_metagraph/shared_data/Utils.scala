package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.types.Codecs._
import com.my.dor_metagraph.shared_data.types.Types.DeviceCheckInInfo
import io.bullet.borer.Cbor
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import _root_.cats.effect.IO
import org.tessellation.security.SecurityProvider

object Utils {
  private val utils = Utils()

  def toTokenAmountFormat(balance: Double): Long = {
    utils.toTokenAmountFormat(balance)
  }

  def getDeviceCheckInInfo(cborData: String): DeviceCheckInInfo = {
    utils.getDeviceCheckInInfo(cborData)
  }

  def convertBytesToHex(bytes: Array[Byte]): String = {
    utils.convertBytesToHex(bytes)
  }

  def getByteArrayFromRequestBody(bodyAsString: String): Array[Byte] = {
    utils.getByteArrayFromRequestBody(bodyAsString)
  }

  def getDagAddressFromPublicKey(publicKey: String, securityProvider: SecurityProvider[IO]): IO[Address] = {
    utils.getDagAddressFromPublicKey(publicKey, securityProvider)
  }

  def toCBORHex(hexString: String): Array[Byte] = {
    utils.toCBORHex(hexString)
  }
}

case class Utils() {
  private val logger = LoggerFactory.getLogger(classOf[Utils])

  def toCBORHex(hexString: String): Array[Byte] = {
    try {
      if ((hexString.length & 1) != 0) {
        logger.error("string length is not even")
        throw new Exception("string length is not even")
      }
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    } catch {
      case NonFatal(e) =>
        logger.error(e.getMessage)
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
    }
  }

  def toTokenAmountFormat(balance: Double): Long = {
    (balance * 10e7).toLong
  }

  def getDeviceCheckInInfo(cborData: String): DeviceCheckInInfo = {
    val checkInCborData = toCBORHex(cborData)
    val decodedCheckIn = Cbor.decode(checkInCborData).to[DeviceCheckInInfo].value

    logger.info(s"Decoded check-in AC: ${decodedCheckIn.ac}")
    logger.info(s"Decoded check-in DTS: ${decodedCheckIn.dts}")
    logger.info(s"Decoded check-in E: ${decodedCheckIn.e}")

    decodedCheckIn
  }

  def convertBytesToHex(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  def getByteArrayFromRequestBody(bodyAsString: String): Array[Byte] = {
    val bodyAsBytes: ListBuffer[Byte] = ListBuffer.empty

    var idx = 0
    while (idx < bodyAsString.length) {
      val substringParsed = bodyAsString.substring(idx, idx + 2).trim
      val parsedString = s"0x$substringParsed"
      bodyAsBytes.addOne(Integer.decode(parsedString).toByte)
      idx = idx + 2
    }

    bodyAsBytes.toArray
  }

  def getDagAddressFromPublicKey(publicKeyHex: String, securityProvider: SecurityProvider[IO]): IO[Address] = {
    implicit val sp: SecurityProvider[IO] = securityProvider
    val publicKey: Id = Id(Hex(publicKeyHex))
    publicKey.toAddress[IO]
  }
}
