package com.my.dor_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.l0.rewards.MainRewards
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedState
import com.my.dor_metagraph.shared_data.decoders.Decoders
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, Encoder}
import org.http4s.{EntityDecoder, HttpRoutes}
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import org.tessellation.currency.dataApplication.{BaseDataApplicationL0Service, DataApplicationL0Service, DataState, DataUpdate, L0NodeContext}
import org.tessellation.currency.l0.CurrencyL0App
import org.tessellation.currency.l0.snapshot.CurrencySnapshotEvent
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.security.SecurityProvider
import org.tessellation.security.hash.Hash
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
    Option(BaseDataApplicationL0Service(new DataApplicationL0Service[IO, CheckInUpdate, CheckInStateOnChain, CheckInDataCalculatedState] {
      override def genesis: DataState[CheckInStateOnChain, CheckInDataCalculatedState] = DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(Map.empty, List.empty, List.empty))

      override def validateUpdate(update: CheckInUpdate)(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

      override def validateData(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = Data.validateData(state, updates)

      override def combine(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = Data.combine(state, updates)

      override def routes(implicit context: L0NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.empty

      override def dataEncoder: Encoder[CheckInUpdate] = implicitly[Encoder[CheckInUpdate]]

      override def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] = implicitly[Encoder[CheckInDataCalculatedState]]

      override def dataDecoder: Decoder[CheckInUpdate] = implicitly[Decoder[CheckInUpdate]]

      override def calculatedStateDecoder: Decoder[CheckInDataCalculatedState] = implicitly[Decoder[CheckInDataCalculatedState]]

      override def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] = Decoders.signedDataEntityDecoder

      override def serializeBlock(block: Signed[DataApplicationBlock]): IO[Array[Byte]] = IO(Serializers.serializeBlock(block)(dataEncoder.asInstanceOf[Encoder[DataUpdate]]))

      override def deserializeBlock(bytes: Array[Byte]): IO[Either[Throwable, Signed[DataApplicationBlock]]] = IO(Deserializers.deserializeBlock(bytes)(dataEncoder.asInstanceOf[Decoder[DataUpdate]]))

      override def serializeState(state: CheckInStateOnChain): IO[Array[Byte]] = IO(Serializers.serializeState(state))

      override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInStateOnChain]] = IO(Deserializers.deserializeState(bytes))

      override def serializeUpdate(update: CheckInUpdate): IO[Array[Byte]] = IO(Serializers.serializeUpdate(update))

      override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, CheckInUpdate]] = IO(Deserializers.deserializeUpdate(bytes))

      override def getCalculatedState(implicit context: L0NodeContext[IO]): IO[(SnapshotOrdinal, CheckInDataCalculatedState)] = CalculatedState.getCalculatedState

      override def setCalculatedState(ordinal: SnapshotOrdinal, state: CheckInDataCalculatedState)(implicit context: L0NodeContext[IO]): IO[Boolean] = CalculatedState.setCalculatedState(ordinal, state)

      override def hashCalculatedState(state: CheckInDataCalculatedState)(implicit context: L0NodeContext[IO]): IO[Hash] = CalculatedState.hashCalculatedState(state)
    }))

  def rewards(implicit sp: SecurityProvider[IO]): Option[Rewards[IO, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent]] = Some(
    MainRewards.make[IO]
  )
}
