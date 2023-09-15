package com.my.dor_metagraph.data_l1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.data_l1.CustomRoutes.{getLatestSnapshotDecoded, getSnapshotByOrdinalDecoded}
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInWithSignature}
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.dsl.io._
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.currency.dataApplication.{BaseDataApplicationL1Service, DataApplicationL1Service, L1NodeContext}
import org.tessellation.currency.l1.CurrencyL1App
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.security.signature.Signed
import org.http4s.EntityDecoder

import java.util.UUID

object Main
  extends CurrencyL1App(
    "currency-data_l1",
    "currency data L1 node",
    ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
    version = BuildInfo.version
  ) {
  override def dataApplication: Option[BaseDataApplicationL1Service[IO]] = Option(BaseDataApplicationL1Service(new DataApplicationL1Service[IO, DeviceCheckInWithSignature, CheckInState] {

    override def serializeState(state: CheckInState): IO[Array[Byte]] = Data.serializeState(state)

    override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInState]] = Data.deserializeState(bytes)

    override def serializeUpdate(update: DeviceCheckInWithSignature): IO[Array[Byte]] = Data.serializeUpdate(update)

    override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceCheckInWithSignature]] = Data.deserializeUpdate(bytes)

    override def dataEncoder: Encoder[DeviceCheckInWithSignature] = Data.dataEncoder

    override def dataDecoder: Decoder[DeviceCheckInWithSignature] = Data.dataDecoder

    override def validateData(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

    override def validateUpdate(update: DeviceCheckInWithSignature)(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = Data.validateUpdate(update)

    override def combine(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L1NodeContext[IO]): IO[CheckInState] = IO.pure(oldState)

    override def routes(implicit context: L1NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.of {
      case GET -> Root / "snapshots" / "latest" => getLatestSnapshotDecoded
      case GET -> Root / "snapshots" / ordinal => getSnapshotByOrdinalDecoded(ordinal)
    }

    override def signedDataEntityDecoder: EntityDecoder[IO, Signed[DeviceCheckInWithSignature]] = Data.signedDataEntityDecoder
  }))
}
