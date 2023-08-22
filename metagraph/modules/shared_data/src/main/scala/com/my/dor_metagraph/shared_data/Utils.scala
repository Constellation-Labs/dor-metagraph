package com.my.dor_metagraph.shared_data

import Codecs._
import Types.{DeviceCheckInInfo, DeviceCheckInWithSignature, CheckInState}
import cats.data.NonEmptySet
import io.bullet.borer.Cbor
import io.circe.parser
import io.circe.syntax.EncoderOps
import org.tessellation.schema.ID.Id
import org.tessellation.security.hash.Hash
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.SortedSet
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

  def getCheckInHash(update: DeviceCheckInWithSignature): String = Hash.fromBytes(customUpdateSerialization(update)).toString

  def customUpdateSerialization(update: DeviceCheckInWithSignature): Array[Byte] = {
    println("Serialize UPDATE event received")
    val cborBytes = toCBORHex(update.cbor)
    println(cborBytes.mkString("Array(", ", ", ")"))
    cborBytes
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceCheckInWithSignature] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      println("KAKAKA")
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

    val hexId = Hex("e75a6011eaa38d7b0a1cb41810c655cdc89c6c5ffd207cbab9d18fd49cbf2729e262b5387a4687a23a163d14bc0dff8ef6539e2a73932e77d2de6b1895facd99")
    val hexSignature = Hex("3044022075d21ce2bc7b247ba987bae12709a35b079c146d9eed2e028c5d96c1e07f9a0b02202689e4f625602b77723698ef3f9bc852dfdcb2f6a90ba562de090dfafbc1237d")

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    Signed(decodedCheckInWithSignature, proofs)
  }
}
