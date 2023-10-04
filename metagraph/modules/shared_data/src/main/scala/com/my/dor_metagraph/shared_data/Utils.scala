package com.my.dor_metagraph.shared_data

import Codecs._
import Types.{CheckInState, DeviceCheckInInfo, DeviceCheckInWithSignature}
import cats.data.NonEmptySet
import io.bullet.borer.Cbor
import io.circe.parser
import io.circe.syntax.EncoderOps
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}

import java.nio.charset.StandardCharsets
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bitcoinj.core.Base58

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.{Address, DAGAddressRefined}
import eu.timepit.refined._

object Utils {
  private val utils = Utils()

  def customUpdateSerialization(update: DeviceCheckInWithSignature): Array[Byte] = {
    utils.customUpdateSerialization(update)
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceCheckInWithSignature] = {
    utils.customUpdateDeserialization(bytes)
  }

  def customStateSerialization(state: CheckInState): Array[Byte] = {
    utils.customStateSerialization(state)
  }

  def customStateDeserialization(bytes: Array[Byte]): Either[Throwable, CheckInState] = {
    utils.customStateDeserialization(bytes)
  }

  def toTokenAmountFormat(balance: Double): Long = {
    utils.toTokenAmountFormat(balance)
  }

  def getDeviceCheckInInfo(cborData: String): DeviceCheckInInfo = {
    utils.getDeviceCheckInInfo(cborData)
  }

  def convertBytesToHex(bytes: Array[Byte]): String = {
    utils.convertBytesToHex(bytes)
  }

  def buildSignedUpdate(cborData: Array[Byte]): Signed[DeviceCheckInWithSignature] = {
    utils.buildSignedUpdate(cborData)
  }

  def getByteArrayFromRequestBody(bodyAsString: String): Array[Byte] = {
    utils.getByteArrayFromRequestBody(bodyAsString)
  }

  def getDagAddressFromPublicKey(publicKey: String): Address = {
    utils.getDagAddressFromPublicKey(publicKey)
  }

}

case class Utils() {
  private val PKCS_PREFIX = "3056301006072a8648ce3d020106052b8104000a034200"
  private val logger = LoggerFactory.getLogger(classOf[Utils])

  private def toCBORHex(hexString: String): Array[Byte] = {
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

  def customUpdateSerialization(update: DeviceCheckInWithSignature): Array[Byte] = {
    logger.info("Serialize UPDATE event received")
    toCBORHex(update.cbor)
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceCheckInWithSignature] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      logger.info(json.toString())
      json.as[DeviceCheckInWithSignature]
    }
  }

  def customStateSerialization(state: CheckInState): Array[Byte] = {
    logger.info("Serialize STATE event received")
    logger.info(state.asJson.deepDropNullValues.noSpaces)
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customStateDeserialization(bytes: Array[Byte]): Either[Throwable, CheckInState] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[CheckInState]
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

  def buildSignedUpdate(cborData: Array[Byte]): Signed[DeviceCheckInWithSignature] = {
    val decodedCheckInWithSignature = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value

    logger.info(s"Decoded CBOR field ${decodedCheckInWithSignature.cbor}")
    logger.info(s"Decoded ID field ${decodedCheckInWithSignature.id}")
    logger.info(s"Decoded SIGNATURE field ${decodedCheckInWithSignature.sig}")

    val hexId = Hex(decodedCheckInWithSignature.id)
    val hexSignature = Hex(decodedCheckInWithSignature.sig)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    Signed(decodedCheckInWithSignature, proofs)
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

  private def sha256(input: String): Array[Byte] = {
    val digest = new SHA256Digest
    val inputBytes = org.bouncycastle.util.encoders.Hex.decode(input)

    digest.update(inputBytes, 0, inputBytes.length)

    val result = new Array[Byte](digest.getDigestSize)
    digest.doFinal(result, 0)

    result
  }

  def getDagAddressFromPublicKey(publicKeyHex: String): Address = {
    var modifiedPublicKeyHex = publicKeyHex
    if (publicKeyHex.length == 128) {
      modifiedPublicKeyHex = "04" + publicKeyHex
    }

    val publicKeyHexWithPrefix = PKCS_PREFIX + modifiedPublicKeyHex
    val sha256Bytes = sha256(publicKeyHexWithPrefix)
    val sha256Str = org.bouncycastle.util.encoders.Hex.toHexString(sha256Bytes)

    val bytes = org.bouncycastle.util.encoders.Hex.decode(sha256Str)
    val hash = Base58.encode(bytes)

    val end = hash.substring(hash.length - 36)
    val sum = end.filter(_.isDigit).map(_.asDigit).sum

    val par = sum % 9
    refineV[DAGAddressRefined](s"DAG$par$end") match {
      case Left(_) => throw new Exception("Error when parsing pub_id to DAG Address")
      case Right(value) => Address(value)
    }
  }
}
