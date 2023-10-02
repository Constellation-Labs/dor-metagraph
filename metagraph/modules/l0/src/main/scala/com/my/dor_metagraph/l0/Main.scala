package com.my.dor_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInWithSignature}
import io.circe.{Decoder, Encoder}
import org.http4s.{EntityDecoder, HttpRoutes}
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.currency.dataApplication.{BaseDataApplicationL0Service, DataApplicationL0Service, L0NodeContext}
import org.tessellation.currency.l0.CurrencyL0App
import org.tessellation.currency.l0.snapshot.CurrencySnapshotEvent
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

import java.util.UUID

object Main
  extends CurrencyL0App(
    "currency-l0",
    "currency L0 node",
    ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
    version = BuildInfo.version
  ) {
  def dataApplication: Option[BaseDataApplicationL0Service[IO]] =
    Option(BaseDataApplicationL0Service(new DataApplicationL0Service[IO, DeviceCheckInWithSignature, CheckInState] {
      override def genesis: CheckInState = CheckInState(List.empty, Map.empty)

      override def validateData(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = Data.validateData(oldState, updates)

      override def validateUpdate(update: DeviceCheckInWithSignature)(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

      def combine(oldState: CheckInState, updates: List[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[CheckInState] = Data.combine(oldState, updates)

      override def serializeState(state: CheckInState): IO[Array[Byte]] = Data.serializeState(state)

      override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInState]] = Data.deserializeState(bytes)

      override def serializeUpdate(update: DeviceCheckInWithSignature): IO[Array[Byte]] = Data.serializeUpdate(update)

      override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceCheckInWithSignature]] = Data.deserializeUpdate(bytes)

      override def dataEncoder: Encoder[DeviceCheckInWithSignature] = Data.dataEncoder

      override def dataDecoder: Decoder[DeviceCheckInWithSignature] = Data.dataDecoder

      override def routes(implicit context: L0NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.empty

      override def signedDataEntityDecoder: EntityDecoder[IO, Signed[DeviceCheckInWithSignature]] = Data.signedDataEntityDecoder
    }))

  def rewards(implicit sp: SecurityProvider[IO]): Option[Rewards[IO, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent]]= Some(
    DorMetagraphRewards.make[IO]
  )
}
