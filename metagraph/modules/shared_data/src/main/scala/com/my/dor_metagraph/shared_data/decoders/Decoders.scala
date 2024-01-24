package com.my.dor_metagraph.shared_data.decoders

import cats.data.NonEmptySet
import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.shared_data.Utils.{getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.external_apis.DorApi.handleCheckInDorApi
import com.my.dor_metagraph.shared_data.types.Codecs._
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.SortedSet


object Decoders {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Decoders")

  private def buildSignedUpdate[F[_] : Async: Env](cborData: Array[Byte]): F[Signed[CheckInUpdate]] = {
    val decodedCheckInWithSignature = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value
    val hexId = Hex(decodedCheckInWithSignature.id)
    val hexSignature = Hex(decodedCheckInWithSignature.sig)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    for {
      _ <- logger.info(s"Decoded CBOR field ${decodedCheckInWithSignature.cbor}")
      _ <- logger.info(s"Decoded HASH field ${decodedCheckInWithSignature.hash}")
      _ <- logger.info(s"Decoded ID field ${decodedCheckInWithSignature.id}")
      _ <- logger.info(s"Decoded SIGNATURE field ${decodedCheckInWithSignature.sig}")
      maybeDeviceCheckInDORApi <- handleCheckInDorApi(decodedCheckInWithSignature.id, decodedCheckInWithSignature)
      checkInInfo <- getDeviceCheckInInfo(decodedCheckInWithSignature.cbor)

      checkInUpdate = CheckInUpdate(
        decodedCheckInWithSignature.id,
        decodedCheckInWithSignature.sig,
        checkInInfo.dts,
        decodedCheckInWithSignature.hash,
        maybeDeviceCheckInDORApi
      )
    } yield Signed(checkInUpdate, proofs)

  }

  def signedDataEntityDecoder[F[_] : Async: Env]: EntityDecoder[F, Signed[CheckInUpdate]] = {
    EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
      val rawText = msg.as[String]
      val signed = rawText.flatMap { text =>
        logger.info(s"Received RAW request: $text")
        val bodyAsBytes = getByteArrayFromRequestBody(text)
        buildSignedUpdate(bodyAsBytes)
      }
      DecodeResult.success(signed)
    }
  }

}