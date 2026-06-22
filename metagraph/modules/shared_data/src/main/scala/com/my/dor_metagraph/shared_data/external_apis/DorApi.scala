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
import scala.util.Random

/**
  * Client for the DOR check-in API.
  *
  * IMPORTANT: this is invoked only at data-L1 ingress (Decoders.buildSignedUpdate), while a
  * submitted check-in is decoded into a Signed[CheckInUpdate]. It is NOT on the consensus path:
  * `combine` never calls it — the decoded response is carried on the signed update and every L0
  * validator reuses that value. Therefore the (blocking) HTTP call, its retries, and the jitter
  * below are per-node, per-submission and cannot cause cross-node divergence / forks.
  */
object DorApi {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("DorApi")

  private val ConnectTimeoutMs: Int = 5_000
  private val ReadTimeoutMs: Int = 10_000
  private val MaxRetries: Int = 3
  private val InitialBackoff: FiniteDuration = 200.millis
  private val MaxBackoff: FiniteDuration = 2.seconds

  // Raised only for transient/retryable conditions: a 5xx, a network/timeout failure, or a 2xx
  // body that failed to decode (treated as transient schema/serialization noise, distinct from a
  // genuine "device not registered" 4xx). A 4xx is NOT an error — it returns None so the update is
  // cleanly rejected as DeviceNotRegisteredOnDorApi without wasting retries.
  private final case class RetryableDorError(message: String) extends RuntimeException(message)

  private def saveDeviceCheckIn[F[_] : Async: Env](
    publicKey      : String,
    deviceCheckIn  : DeviceCheckInWithSignature,
    checkInInfo    : DeviceCheckInInfo
  ): F[Option[DorAPIResponse]] =
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

      // check = false: inspect the status code ourselves instead of letting the client throw on
      // any non-2xx, so we can distinguish retryable (5xx/decode) from terminal (4xx) outcomes.
      response <- Async[F].blocking {
        val r = requests.post(
          url = endpoint,
          headers = headers,
          data = requestBody,
          readTimeout = ReadTimeoutMs,
          connectTimeout = ConnectTimeoutMs,
          check = false
        )
        (r.statusCode, r.text())
      }

      (statusCode, body) = response

      decodedResponse <- statusCode match {
        case code if code >= 200 && code < 300 =>
          decode[DorAPIResponse](body).fold(
            err =>
              logger.warn(s"DOR API 2xx body for $publicKey failed to decode (length=${body.length}): ${err.getMessage}") >>
                RetryableDorError(s"undecodable 2xx response for $publicKey").raiseError[F, Option[DorAPIResponse]],
            response => logger.debug(s"DOR API check-in accepted for $publicKey").as(response.some)
          )

        case code if code >= 400 && code < 500 =>
          logger
            .info(s"DOR API returned $code for $publicKey; treating device as not registered (no retry)")
            .as(none[DorAPIResponse])

        case code =>
          logger.warn(s"DOR API returned $code for $publicKey (retryable)") >>
            RetryableDorError(s"server error $code for $publicKey").raiseError[F, Option[DorAPIResponse]]
      }
    } yield decodedResponse

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
        // Full jitter on top of capped exponential backoff to avoid synchronized retry storms when
        // DOR is degraded. Randomness is safe here: ingress-only, not re-executed during consensus.
        val jitterMs = Random.nextLong(backoff.toMillis + 1L)
        val delay = (backoff + jitterMs.millis).min(MaxBackoff)
        val nextBackoff = (backoff * 2).min(MaxBackoff)
        logger.warn(s"DOR API call for $publicKey failed on attempt $attempt: ${err.getMessage}. Retrying in $delay") >>
          Async[F].sleep(delay) >>
          retrying(publicKey, attempt + 1, nextBackoff, action)
      }
    }

  def handleCheckInDorApi[F[_] : Async: Env](
    publicKey    : String,
    deviceCheckIn: DeviceCheckInWithSignature,
    checkInInfo  : DeviceCheckInInfo
  ): F[Option[DorAPIResponse]] =
    retrying(publicKey, attempt = 1, backoff = InitialBackoff, saveDeviceCheckIn(publicKey, deviceCheckIn, checkInInfo))
}
