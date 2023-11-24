package com.my.dor_metagraph.shared_data.decoders

import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits._
import com.my.dor_metagraph.shared_data.Utils.{getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.external_apis.DorApi.saveDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import org.http4s.{DecodeResult, EntityDecoder}
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import com.my.dor_metagraph.shared_data.types.Codecs._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.SortedSet


object Decoders {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Decoders")

  private def buildSignedUpdate[F[_] : Async](
    cborData  : Array[Byte],
    rawRequest: String
  ): F[Signed[CheckInUpdate]] = {
    val decodedCheckInWithSignature = Cbor.decode(cborData).to[DeviceCheckInWithSignature].value
    val hexId = Hex(decodedCheckInWithSignature.id)
    val hexSignature = Hex(decodedCheckInWithSignature.sig)

    val signatureProof = SignatureProof(Id(hexId), Signature(hexSignature))
    val proofs = NonEmptySet.fromSetUnsafe(SortedSet(signatureProof))

    for {
      _ <- logger.debug(s"RAW Request: $rawRequest")
      _ <- logger.debug(s"Decoded CBOR field ${decodedCheckInWithSignature.cbor}")
      _ <- logger.debug(s"Decoded HASH field ${decodedCheckInWithSignature.hash}")
      _ <- logger.debug(s"Decoded ID field ${decodedCheckInWithSignature.id}")
      _ <- logger.debug(s"Decoded SIGNATURE field ${decodedCheckInWithSignature.sig}")
      deviceCheckInDORApi <- saveDeviceCheckIn(decodedCheckInWithSignature.id, decodedCheckInWithSignature)
        .flatMap(value => value.some.pure[F])
        .handleErrorWith { e =>
          logger.warn(s"Error when fetching DOR API. Message ${e.getMessage}") >> none[DorAPIResponse].pure[F]
        }
      checkInInfo <- getDeviceCheckInInfo(decodedCheckInWithSignature.cbor)
      checkInUpdate = CheckInUpdate(
        decodedCheckInWithSignature.id,
        decodedCheckInWithSignature.sig,
        checkInInfo.dts,
        decodedCheckInWithSignature.hash,
        deviceCheckInDORApi
      )
    } yield Signed(checkInUpdate, proofs)
  }

  def signedDataEntityDecoder[F[_] : Async]: EntityDecoder[F, Signed[CheckInUpdate]] =
    EntityDecoder.text[F].flatMapR { text =>
      val bodyAsBytes = getByteArrayFromRequestBody(text)
      val signedUpdateF = buildSignedUpdate(bodyAsBytes, text)
      DecodeResult.success(signedUpdateF)
    }
}