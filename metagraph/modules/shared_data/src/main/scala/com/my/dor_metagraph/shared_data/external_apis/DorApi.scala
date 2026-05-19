package com.my.dor_metagraph.shared_data.external_apis

import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import com.my.dor_metagraph.shared_data.Utils.getEnv
import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInInfo, DeviceCheckInWithSignature, DorAPIResponse}
import io.circe.parser.decode
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import ujson.Obj

import scala.concurrent.duration._

object DorApi {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("DorApi")

  private val ConnectTimeoutMs: Int = 5_000
  private val ReadTimeoutMs: Int = 10_000
  private val MaxRetries: Int = 3
  private val InitialBackoff: FiniteDuration = 200.millis

  private def saveDeviceCheckIn[F[_] : Async: Env](
    publicKey      : String,
    deviceCheckIn  : DeviceCheckInWithSignature,
    checkInInfo    : DeviceCheckInInfo
  ): F[Option[DorAPIResponse]] = {
    for {
      apiUrl <- getEnv[F]("DOR_API_URL")
      endpoint = s"$apiUrl/$publicKey/check-in"
      headers = Map("Content-Type" -> "application/json", "version" -> "2")

      requestBody = Obj(
        "ac" -> checkInInfo.ac,
        "dts" -> checkInInfo.dts,
        "e" -> checkInInfo.e,
        "hash" -> deviceCheckIn.hash,
        "signature" -> deviceCheckIn.sig
      ).render()

      _ <- logger.debug(s"Posting check-in to DOR for $publicKey")

      body <- Async[F].blocking(
        requests.post(
          url = endpoint,
          headers = headers,
          data = requestBody,
          readTimeout = ReadTimeoutMs,
          connectTimeout = ConnectTimeoutMs
        ).text()
      )

      _ <- logger.debug(s"DOR API response for $publicKey: $body")

      decodedResponse <- decode[DorAPIResponse](body).fold(
        err => logger.warn(s"Failing when decoding DOR response for $publicKey: ${err.getMessage}").as(none[DorAPIResponse]),
        response => Async[F].pure(response.some)
      )
    } yield decodedResponse
  }

  private def retrying[F[_] : Async, A](
    publicKey  : String,
    attempt    : Int,
    backoff    : FiniteDuration,
    action     : F[A]
  ): F[A] =
    action.handleErrorWith { err =>
      if (attempt >= MaxRetries) {
        logger.warn(s"DOR API call for $publicKey failed after $MaxRetries attempts: ${err.getMessage}") >>
          err.raiseError[F, A]
      } else {
        logger.warn(s"DOR API call for $publicKey failed on attempt $attempt: ${err.getMessage}. Retrying in $backoff") >>
          Async[F].sleep(backoff) >>
          retrying(publicKey, attempt + 1, backoff * 2, action)
      }
    }

  def handleCheckInDorApi[F[_] : Async: Env](
    publicKey    : String,
    deviceCheckIn: DeviceCheckInWithSignature,
    checkInInfo  : DeviceCheckInInfo
  ): F[Option[DorAPIResponse]] =
    retrying(publicKey, attempt = 1, backoff = InitialBackoff, saveDeviceCheckIn(publicKey, deviceCheckIn, checkInInfo))
}
