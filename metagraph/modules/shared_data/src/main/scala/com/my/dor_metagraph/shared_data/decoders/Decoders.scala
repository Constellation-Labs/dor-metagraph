package com.my.dor_metagraph.shared_data.decoders

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.Utils.{getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.external_apis.DorApi.saveDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.slf4j.LoggerFactory
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import com.my.dor_metagraph.shared_data.types.Codecs._

import scala.collection.immutable.SortedSet


object Decoders {
  private val decoders: Decoders = Decoders()
  def dataDecoder: Decoder[CheckInUpdate] = deriveDecoder
  def calculatedStateDecoder: Decoder[CheckInDataCalculatedState] = deriveDecoder

  def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] = {
    decoders.signedDataEntityDecoder
  }
}

case class Decoders() {
  private val logger = LoggerFactory.getLogger(classOf[Decoders])

  private def buildSignedUpdate(cborData: Array[Byte]): Signed[CheckInUpdate] = {
    val decodedCheckInWithSignature = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value

    logger.info(s"Decoded CBOR field ${decodedCheckInWithSignature.cbor}")
    logger.info(s"Decoded ID field ${decodedCheckInWithSignature.id}")
    logger.info(s"Decoded SIGNATURE field ${decodedCheckInWithSignature.sig}")

    val hexId = Hex(decodedCheckInWithSignature.id)
    val hexSignature = Hex(decodedCheckInWithSignature.sig)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    saveDeviceCheckIn(decodedCheckInWithSignature.id, decodedCheckInWithSignature) match {
      case Some(value) =>
        val checkInInfo = getDeviceCheckInInfo(decodedCheckInWithSignature.cbor)
        val checkInUpdate = CheckInUpdate(
          decodedCheckInWithSignature.id,
          decodedCheckInWithSignature.sig,
          checkInInfo.dts,
          decodedCheckInWithSignature.cbor,
          value
        )
        Signed(checkInUpdate, proofs)
      case None =>
        logger.error(s"Error when making check-in with device: ${decodedCheckInWithSignature.id}")
        throw new Exception(s"Error when making check-in with device: ${decodedCheckInWithSignature.id}")
    }
  }
  def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] = {
    EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
      val rawText = msg.as[String]
      val signed = rawText.flatMap { text =>
        logger.info(s"Received RAW request: $text")
        val bodyAsBytes = getByteArrayFromRequestBody(text)
        buildSignedUpdate(bodyAsBytes).pure[IO]
      }
      logger.info(s"PARSED RAW request: $signed")
      DecodeResult.success(signed)
    }
  }

}
