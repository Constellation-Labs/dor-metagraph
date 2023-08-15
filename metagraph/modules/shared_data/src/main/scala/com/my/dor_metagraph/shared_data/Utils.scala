package com.my.dor_metagraph.shared_data

import Data.{DeviceCheckInRawUpdate, DeviceCheckInWithSignature, State}
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
import scala.util.control.NonFatal

object Utils {
  def toCBORHex(hexString: String): Array[Byte] = {
    try {
      if ((hexString.length & 1) != 0) sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    } catch {
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
    }
  }

  def customUpdateSerialization(update: DeviceCheckInRawUpdate): Array[Byte] = {
    println("Serialize UPDATE event received")
    println(update.asJson.deepDropNullValues.noSpaces)
    update.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customStateSerialization(state: State): Array[Byte] = {
    println("Serialize STATE event received")
    println(state.asJson.deepDropNullValues.noSpaces)
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customStateDeserialization(bytes: Array[Byte]): Either[Throwable, State] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[State]
    }
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceCheckInRawUpdate] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[DeviceCheckInRawUpdate]
    }
  }

  def toTokenAmountFormat(balance: Double): Long = {
    (balance * 10e7).toLong
  }

  def buildSignedUpdate(text: String): Signed[DeviceCheckInRawUpdate] = {
    val cborData = Utils.toCBORHex(text)
    val decodedCheckIn = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value

    val deviceUpdate = DeviceCheckInRawUpdate(decodedCheckIn.ac, decodedCheckIn.dts, decodedCheckIn.e)

    val hexId = Hex(decodedCheckIn.id)
    val hexSignature = Hex(decodedCheckIn.signature)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    Signed(deviceUpdate, proofs)
  }
}
