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
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

object Utils {
  private def toCBORHex(hexString: String): Array[Byte] = {
    try {
      if ((hexString.length & 1) != 0) sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    } catch {
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
    }
  }
  def customUpdateSerialization(update: DeviceCheckInWithSignature): Array[Byte] = {
    println("Serialize UPDATE event received")
    toCBORHex(update.cbor)
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceCheckInWithSignature] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      println(json)
      json.as[DeviceCheckInWithSignature]
    }
  }

  def customStateSerialization(state: CheckInState): Array[Byte] = {
    println("Serialize STATE event received")
    println(state.asJson.deepDropNullValues.noSpaces)
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
    val checkInCborData = Utils.toCBORHex(cborData)
    val decodedCheckIn = Cbor.decode(checkInCborData).to[DeviceCheckInInfo].value

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
}
