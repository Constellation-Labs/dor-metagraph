package com.my.dor_metagraph.data_l1

import cats.effect.{IO, Resource}
import cats.syntax.option.catsSyntaxOptionId
import com.my.dor_metagraph.shared_data.LifecycleSharedFunctions
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.decoders.Decoders
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, Encoder}
import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import io.constellationnetwork.currency.l1.CurrencyL1App
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.security.signature.Signed
import org.http4s.{EntityDecoder, HttpRoutes}

import java.util.UUID

object Main extends CurrencyL1App(
  "currency-data_l1",
  "currency data L1 node",
  ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
  tessellationVersion = TessellationVersion.unsafeFrom(io.constellationnetwork.BuildInfo.version),
  metagraphVersion = MetagraphVersion.unsafeFrom(com.my.dor_metagraph.data_l1.BuildInfo.version)
) {

  private def makeBaseDataApplicationL1Service(
    calculatedStateService: CalculatedStateService[IO]
  ): BaseDataApplicationL1Service[IO] = BaseDataApplicationL1Service(
    new DataApplicationL1Service[IO, CheckInUpdate, CheckInStateOnChain, CheckInDataCalculatedState] {
      override def validateUpdate(
        update: CheckInUpdate
      )(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
        LifecycleSharedFunctions.validateUpdate[IO](update)

      override def routes(implicit context: L1NodeContext[IO]): HttpRoutes[IO] =
        HttpRoutes.empty

      override def dataEncoder: Encoder[CheckInUpdate] =
        implicitly[Encoder[CheckInUpdate]]

      override def dataDecoder: Decoder[CheckInUpdate] =
        implicitly[Decoder[CheckInUpdate]]

      override def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] =
        implicitly[Encoder[CheckInDataCalculatedState]]

      override def calculatedStateDecoder: Decoder[CheckInDataCalculatedState] =
        implicitly[Decoder[CheckInDataCalculatedState]]

      override def signedDataEntityDecoder: EntityDecoder[IO, Signed[CheckInUpdate]] =
        Decoders.signedDataEntityDecoder

      override def serializeBlock(
        block: Signed[DataApplicationBlock]
      ): IO[Array[Byte]] =
        IO(Serializers.serializeBlock(block)(dataEncoder.asInstanceOf[Encoder[DataUpdate]]))

      override def deserializeBlock(
        bytes: Array[Byte]
      ): IO[Either[Throwable, Signed[DataApplicationBlock]]] =
        IO(Deserializers.deserializeBlock(bytes)(dataDecoder.asInstanceOf[Decoder[DataUpdate]]))

      override def serializeState(
        state: CheckInStateOnChain
      ): IO[Array[Byte]] =
        IO(Serializers.serializeState(state))

      override def deserializeState(
        bytes: Array[Byte]
      ): IO[Either[Throwable, CheckInStateOnChain]] =
        IO(Deserializers.deserializeState(bytes))

      override def serializeUpdate(
        update: CheckInUpdate
      ): IO[Array[Byte]] =
        IO(Serializers.serializeUpdate(update))

      override def deserializeUpdate(
        bytes: Array[Byte]
      ): IO[Either[Throwable, CheckInUpdate]] =
        IO(Deserializers.deserializeUpdate(bytes))

      override def serializeCalculatedState(
        state: CheckInDataCalculatedState
      ): IO[Array[Byte]] =
        IO(Serializers.serializeCalculatedState(state))

      override def deserializeCalculatedState(
        bytes: Array[Byte]
      ): IO[Either[Throwable, CheckInDataCalculatedState]] =
        IO(Deserializers.deserializeCalculatedState(bytes))
    }
  )

  private def makeL1Service: IO[BaseDataApplicationL1Service[IO]] = {
    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      dataApplicationL1Service = makeBaseDataApplicationL1Service(calculatedStateService)
    } yield dataApplicationL1Service
  }

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL1Service[IO]]] =
    makeL1Service.asResource.some
}
