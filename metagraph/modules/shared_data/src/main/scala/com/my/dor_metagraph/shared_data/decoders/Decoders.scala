package com.my.dor_metagraph.shared_data.decoders

import cats.data.{EitherT, NonEmptySet}
import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.{getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.external_apis.DorApi.handleCheckInDorApi
import com.my.dor_metagraph.shared_data.types.Codecs._
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import org.http4s.{DecodeFailure, DecodeResult, EntityDecoder, InvalidMessageBodyFailure, MediaType}
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
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

  def signedDataEntityDecoder[F[_]: Async: Env]: EntityDecoder[F, Signed[CheckInUpdate]] = {
    EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
      EitherT {
        (for {
          text <- msg.as[String]
          _ <- logger.info(s"Received RAW request: $text")
          bodyAsBytes = getByteArrayFromRequestBody(text)
          result <- buildSignedUpdate(bodyAsBytes)
        } yield Right(result): Either[DecodeFailure, Signed[CheckInUpdate]]
          ).handleErrorWith { err =>
          (Left(InvalidMessageBodyFailure(s"Failed to decode CheckInUpdate: ${err.getMessage}", err.some)): Either[DecodeFailure, Signed[CheckInUpdate]]).pure[F]
        }
      }
    }
  }

}