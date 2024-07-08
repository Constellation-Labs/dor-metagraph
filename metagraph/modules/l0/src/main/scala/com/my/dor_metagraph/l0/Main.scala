package com.my.dor_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.applicative._
import cats.syntax.option._
import cats.syntax.validated._
import com.my.dor_metagraph.l0.custom_routes.CustomRoutes
import com.my.dor_metagraph.l0.rewards.DorRewards
import com.my.dor_metagraph.l0.rewards.bounties.{AnalyticsBountyRewards, DailyBountyRewards}
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodesAPI
import com.my.dor_metagraph.shared_data.LifecycleSharedFunctions
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.decoders.Decoders
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, Encoder}
import org.http4s.{EntityDecoder, HttpRoutes}
import org.tessellation.currency.dataApplication._
import org.tessellation.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import org.tessellation.currency.l0.CurrencyL0App
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.ext.cats.effect.ResourceIO
import org.tessellation.node.shared.domain.rewards.Rewards
import org.tessellation.node.shared.snapshot.currency.CurrencySnapshotEvent
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.schema.semver.{MetagraphVersion, TessellationVersion}
import org.tessellation.security.SecurityProvider
import org.tessellation.security.hash.Hash
import org.tessellation.security.signature.Signed

import java.util.UUID

object Main extends CurrencyL0App(
  "currency-l0",
  "currency L0 node",
  ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
  tessellationVersion = TessellationVersion.unsafeFrom(org.tessellation.BuildInfo.version),
  metagraphVersion = MetagraphVersion.unsafeFrom(com.my.dor_metagraph.l0.BuildInfo.version)
) {
  private def makeBaseDataApplicationL0Service(
    calculatedStateService: CalculatedStateService[IO]
  ): BaseDataApplicationL0Service[IO] =
    BaseDataApplicationL0Service(
      new DataApplicationL0Service[IO, CheckInUpdate, CheckInStateOnChain, CheckInDataCalculatedState] {
        override def genesis: DataState[CheckInStateOnChain, CheckInDataCalculatedState] =
          DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(Map.empty))

        override def validateUpdate(
          update: CheckInUpdate
        )(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
          ().validNec.pure[IO]

        override def validateData(
          state  : DataState[CheckInStateOnChain, CheckInDataCalculatedState],
          updates: NonEmptyList[Signed[CheckInUpdate]]
        )(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
          LifecycleSharedFunctions.validateData[IO](state, updates)

        override def combine(
          state  : DataState[CheckInStateOnChain, CheckInDataCalculatedState],
          updates: List[Signed[CheckInUpdate]]
        )(implicit context: L0NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] =
          LifecycleSharedFunctions.combine[IO](state, updates)

        override def dataEncoder: Encoder[CheckInUpdate] =
          implicitly[Encoder[CheckInUpdate]]

        override def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] =
          implicitly[Encoder[CheckInDataCalculatedState]]

        override def dataDecoder: Decoder[CheckInUpdate] =
          implicitly[Decoder[CheckInUpdate]]

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

        override def getCalculatedState(implicit context: L0NodeContext[IO]): IO[(SnapshotOrdinal, CheckInDataCalculatedState)] =
          calculatedStateService.getCalculatedState.map(calculatedState => (calculatedState.ordinal, calculatedState.state))

        override def setCalculatedState(
          ordinal: SnapshotOrdinal,
          state  : CheckInDataCalculatedState
        )(implicit context: L0NodeContext[IO]): IO[Boolean] =
          calculatedStateService.setCalculatedState(ordinal, state)

        override def hashCalculatedState(
          state: CheckInDataCalculatedState
        )(implicit context: L0NodeContext[IO]): IO[Hash] =
          calculatedStateService.hashCalculatedState(state)

        override def routes(implicit context: L0NodeContext[IO]): HttpRoutes[IO] =
          CustomRoutes[IO](calculatedStateService).public

        override def serializeCalculatedState(
          state: CheckInDataCalculatedState
        ): IO[Array[Byte]] =
          IO(Serializers.serializeCalculatedState(state))

        override def deserializeCalculatedState(
          bytes: Array[Byte]
        ): IO[Either[Throwable, CheckInDataCalculatedState]] =
          IO(Deserializers.deserializeCalculatedState(bytes))
      })

  private def makeL0Service: IO[BaseDataApplicationL0Service[IO]] = {
    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      dataApplicationL0Service = makeBaseDataApplicationL0Service(calculatedStateService)
    } yield dataApplicationL0Service
  }

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL0Service[IO]]] =
    makeL0Service.asResource.some

  override def rewards(implicit sp: SecurityProvider[IO]): Option[Rewards[IO, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent]] = {
    val dailyBountyRewards = new DailyBountyRewards[IO]
    val analyticsBountyRewards = new AnalyticsBountyRewards[IO]
    val validatorNodes = new ValidatorNodesAPI[IO]

    DorRewards.make[IO](
      dailyBountyRewards,
      analyticsBountyRewards,
      validatorNodes
    ).some
  }
}

