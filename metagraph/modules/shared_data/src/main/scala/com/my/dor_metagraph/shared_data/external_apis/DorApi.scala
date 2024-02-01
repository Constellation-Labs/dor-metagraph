package com.my.dor_metagraph.shared_data.external_apis

import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import com.my.dor_metagraph.shared_data.Utils.{getDeviceCheckInInfo, getEnv}
import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInWithSignature, DorAPIResponse}
import io.circe.parser.decode
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import ujson.Obj

object DorApi {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("DorApi")

  private def saveDeviceCheckIn[F[_] : Async: Env](
    publicKey    : String,
    deviceCheckIn: DeviceCheckInWithSignature
  ): F[Option[DorAPIResponse]] = {
    for {
      apiUrl <- getEnv[F]("DOR_API_URL")
      endpoint = s"$apiUrl/$publicKey/check-in"
      headers = Map("Content-Type" -> "application/json", "version" -> "2")
      checkInInfo <- getDeviceCheckInInfo(deviceCheckIn.cbor)
      _ <- logger.info(s"Decoded CBOR field before check-in to DOR Server AC ${checkInInfo.ac}")
      _ <- logger.info(s"Decoded CBOR field before check-in to DOR Server DTS ${checkInInfo.dts}")
      _ <- logger.info(s"Decoded CBOR field before check-in to DOR Server E ${checkInInfo.e}")

      requestBody = Obj(
        "ac" -> checkInInfo.ac,
        "dts" -> checkInInfo.dts,
        "e" -> checkInInfo.e,
        "hash" -> deviceCheckIn.hash,
        "signature" -> deviceCheckIn.sig
      ).render()

      _ <- logger.info(s"Request body: $requestBody")

      body = requests.post(
        url = endpoint,
        headers = headers,
        data = requestBody
      ).text()

      _ <- logger.info(s"API response $body")

      decodedResponse <- decode[DorAPIResponse](body).fold(
        err => logger.warn(s"Failing when decoding: ${err.getMessage}").as(none),
        response => Async[F].pure(response.some)
      )
    } yield decodedResponse
  }

  def handleCheckInDorApi[F[_] : Async: Env](
    publicKey    : String,
    deviceCheckIn: DeviceCheckInWithSignature
  ): F[Option[DorAPIResponse]] = {
    saveDeviceCheckIn(publicKey, deviceCheckIn).handleErrorWith { err =>
      logger.warn(s"Failing when check in: ${err.getMessage}").as(none)
    }
  }
}
