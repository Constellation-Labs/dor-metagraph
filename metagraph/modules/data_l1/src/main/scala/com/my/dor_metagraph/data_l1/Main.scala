package com.my.dor_metagraph.data_l1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedState
import com.my.dor_metagraph.shared_data.decoders.Decoders
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import org.tessellation.currency.dataApplication.{BaseDataApplicationL1Service, DataApplicationL1Service, DataState, DataUpdate, L1NodeContext}
import org.tessellation.currency.l1.CurrencyL1App
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.security.signature.Signed
import org.http4s.EntityDecoder
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash
import io.circe.generic.auto._

import java.util.UUID

object Main
  extends CurrencyL1App(
    "currency-data_l1",
    "currency data L1 node",
    ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
    version = BuildInfo.version
  ) {
  override def dataApplication: Option[BaseDataApplicationL1Service[IO]] = Option(BaseDataApplicationL1Service(new DataApplicationL1Service[IO, CheckInUpdate, CheckInStateOnChain, CheckInDataCalculatedState] {

    override def validateData(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

    override def validateUpdate(update: CheckInUpdate)(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = Data.validateUpdate(update)

    override def combine(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L1NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = IO.pure(state)

    override def routes(implicit context: L1NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.empty

    override def dataEncoder: Encoder[CheckInUpdate] = implicitly[Encoder[CheckInUpdate]]

    override def dataDecoder: Decoder[CheckInUpdate] = implicitly[Decoder[CheckInUpdate]]

    override def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] = implicitly[Encoder[CheckInDataCalculatedState]]

    override def calculatedStateDecoder: Decoder[CheckInDataCalculatedState] = implicitly[Decoder[CheckInDataCalculatedState]]

    override def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] = Decoders.signedDataEntityDecoder

    override def serializeBlock(block: Signed[DataApplicationBlock]): IO[Array[Byte]] = IO(Serializers.serializeBlock(block)(dataEncoder.asInstanceOf[Encoder[DataUpdate]]))

    override def deserializeBlock(bytes: Array[Byte]): IO[Either[Throwable, Signed[DataApplicationBlock]]] = IO(Deserializers.deserializeBlock(bytes)(dataEncoder.asInstanceOf[Decoder[DataUpdate]]))

    override def serializeState(state: CheckInStateOnChain): IO[Array[Byte]] = IO(Serializers.serializeState(state))

    override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInStateOnChain]] = IO(Deserializers.deserializeState(bytes))

    override def serializeUpdate(update: CheckInUpdate): IO[Array[Byte]] = IO(Serializers.serializeUpdate(update))

    override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, CheckInUpdate]] = IO(Deserializers.deserializeUpdate(bytes))

    override def getCalculatedState(implicit context: L1NodeContext[IO]): IO[(SnapshotOrdinal, CheckInDataCalculatedState)] = CalculatedState.getCalculatedState

    override def setCalculatedState(ordinal: SnapshotOrdinal, state: CheckInDataCalculatedState)(implicit context: L1NodeContext[IO]): IO[Boolean] = CalculatedState.setCalculatedState(ordinal, state)

    override def hashCalculatedState(state: CheckInDataCalculatedState)(implicit context: L1NodeContext[IO]): IO[Hash] = CalculatedState.hashCalculatedState(state)
  }))
}
