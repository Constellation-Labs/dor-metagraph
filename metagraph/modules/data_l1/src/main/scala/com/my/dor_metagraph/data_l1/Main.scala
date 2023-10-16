package com.my.dor_metagraph.data_l1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxValidatedIdBinCompat0}
import com.my.dor_metagraph.shared_data.Data
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedState
import com.my.dor_metagraph.shared_data.decoders.Decoders
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.encoders.Encoders
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate, DeviceCheckInWithSignature}
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.tessellation.BuildInfo
import org.tessellation.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import org.tessellation.currency.dataApplication.{BaseDataApplicationL1Service, DataApplicationL1Service, DataState, L1NodeContext, dataApplication}
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
  override def dataApplication: Option[BaseDataApplicationL1Service[IO]] = Option(BaseDataApplicationL1Service(new DataApplicationL1Service[IO, CheckInUpdate, CheckInStateOnChain, CheckInDataCalculatedState] {

    override def validateData(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO.pure(().validNec)

    override def validateUpdate(update: CheckInUpdate)(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = Data.validateUpdate(update)

    override def combine(state: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L1NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = IO.pure(state)

    override def routes(implicit context: L1NodeContext[IO]): HttpRoutes[IO] = HttpRoutes.empty

    override def dataEncoder: Encoder[CheckInUpdate] = Encoders.dataEncoder
    override def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] = Encoders.calculatedStateEncoder

    override def dataDecoder: Decoder[CheckInUpdate] = Decoders.dataDecoder
    override def calculatedStateDecoder: Decoder[CheckInDataCalculatedState] = Decoders.calculatedStateDecoder
    override def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] = Decoders.signedDataEntityDecoder

    override def serializeBlock(block: DataApplicationBlock): IO[Array[Byte]] = Serializers.serializeBlock(block).pure
    override def deserializeBlock(bytes: Array[Byte]): IO[Either[Throwable, DataApplicationBlock]] = Deserializers.deserializeBlock(bytes).pure

    override def serializeState(state: CheckInStateOnChain): IO[Array[Byte]] = Serializers.serializeState(state).pure
    override def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInStateOnChain]] = Deserializers.deserializeState(bytes).pure

    override def serializeUpdate(update: CheckInUpdate): IO[Array[Byte]] = Serializers.serializeUpdate(update).pure
    override def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, CheckInUpdate]] = Deserializers.deserializeUpdate(bytes).pure

    override def getCalculatedState(implicit context: L1NodeContext[IO]): IO[CheckInDataCalculatedState] = CalculatedState.getCalculatedState
    override def setCalculatedState(state: CheckInDataCalculatedState)(implicit context: L1NodeContext[IO]): IO[Boolean] = CalculatedState.setCalculatedState(state)
  }))
}
