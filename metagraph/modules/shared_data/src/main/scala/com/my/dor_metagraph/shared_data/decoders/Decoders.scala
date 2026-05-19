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
      _ <- logger.debug(s"Decoded check-in for id=${decodedCheckInWithSignature.id}")
      checkInInfo <- getDeviceCheckInInfo(decodedCheckInWithSignature.cbor)
      maybeDeviceCheckInDORApi <- handleCheckInDorApi(decodedCheckInWithSignature.id, decodedCheckInWithSignature, checkInInfo)

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
        val bodyAsBytes = getByteArrayFromRequestBody(text)
        buildSignedUpdate(bodyAsBytes)
      }
      DecodeResult.success(signed)
    }
  }

}