package com.my.dor_metagraph.shared_data.decoders

import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import com.my.dor_metagraph.shared_data.Utils.{getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.external_apis.DorApi.saveDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.slf4j.LoggerFactory
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import com.my.dor_metagraph.shared_data.types.Codecs._

import scala.collection.immutable.SortedSet


object Decoders {
  private val logger = LoggerFactory.getLogger("Decoders")

  private def buildSignedUpdate(cborData: Array[Byte]): Signed[CheckInUpdate] = {
    val decodedCheckInWithSignature = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value

    logger.info(s"Decoded CBOR field ${decodedCheckInWithSignature.cbor}")
    logger.info(s"Decoded HASH field ${decodedCheckInWithSignature.hash}")
    logger.info(s"Decoded ID field ${decodedCheckInWithSignature.id}")
    logger.info(s"Decoded SIGNATURE field ${decodedCheckInWithSignature.sig}")

    val hexId = Hex(decodedCheckInWithSignature.id)
    val hexSignature = Hex(decodedCheckInWithSignature.sig)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    val deviceCheckInDORApi = saveDeviceCheckIn(decodedCheckInWithSignature.id, decodedCheckInWithSignature)
    val checkInInfo = getDeviceCheckInInfo(decodedCheckInWithSignature.cbor)
    val checkInUpdate = CheckInUpdate(
      decodedCheckInWithSignature.id,
      decodedCheckInWithSignature.sig,
      checkInInfo.dts,
      decodedCheckInWithSignature.hash,
      deviceCheckInDORApi
    )

    Signed(checkInUpdate, proofs)
  }

  def signedDataEntityDecoder[F[_] : Async]: EntityDecoder[F, Signed[CheckInUpdate]] = EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
    val signedUpdate = for {
      text <- msg.as[String]
      _ = logger.info(s"Received RAW request: $text")
      bodyAsBytes = getByteArrayFromRequestBody(text)
      signedUpdate = buildSignedUpdate(bodyAsBytes)
      _ = logger.info(s"PARSED RAW request: Value:${signedUpdate.value} Proofs: ${signedUpdate.proofs}")
    } yield signedUpdate

    logger.info("TEST")
    val t = DecodeResult.success(signedUpdate)
    logger.info("TEST2")
    t
  }
}