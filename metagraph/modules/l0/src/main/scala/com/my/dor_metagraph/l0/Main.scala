package com.my.dor_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.Data.{DeviceUpdate, State}
import io.circe.{Decoder, Encoder}
import org.http4s.HttpRoutes
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.currency.dataApplication.{BaseDataApplicationL0Service, DataApplicationL0Service, L0NodeContext}
import org.tessellation.currency.l0.CurrencyL0App
import org.tessellation.currency.schema.currency
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
    Option(BaseDataApplicationL0Service(new DataApplicationL0Service[IO, DeviceUpdate, State] {
      override def genesis: State = State(Map.empty)

      override def validateData(oldState: State, updates: NonEmptyList[Signed[DeviceUpdate]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

      override def validateUpdate(update: DeviceUpdate)(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

      override def combine(oldState: State, updates: NonEmptyList[Signed[DeviceUpdate]])(implicit context: L0NodeContext[IO]): IO[State] = Data.combine(oldState, updates)

      override def serializeState(state: State): IO[Array[Byte]] = Data.serializeState(state)

      override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, State]] = Data.deserializeState(bytes)

      override def serializeUpdate(update: DeviceUpdate): IO[Array[Byte]] = Data.serializeUpdate(update)

      override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceUpdate]] = Data.deserializeUpdate(bytes)

      override def dataEncoder: Encoder[DeviceUpdate] = Data.dataEncoder

      override def dataDecoder: Decoder[DeviceUpdate] = Data.dataDecoder

      override def routes(implicit context: L0NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.empty

    }))

  def rewards(implicit sp: SecurityProvider[IO]): Some[Rewards[IO, currency.CurrencyTransaction, currency.CurrencyBlock, currency.CurrencySnapshotStateProof, currency.CurrencyIncrementalSnapshot]] = Some(
    Rewards.distributeRewards[IO]
  )
}
