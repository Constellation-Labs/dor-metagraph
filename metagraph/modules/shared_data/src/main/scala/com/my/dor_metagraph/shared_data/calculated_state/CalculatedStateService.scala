package com.my.dor_metagraph.shared_data.calculated_state

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxFlatMapOps, toFunctorOps}
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import io.circe.syntax.EncoderOps
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.charset.StandardCharsets

trait CalculatedStateService[F[_]] {
  def getCalculatedState: F[CalculatedState]

  def setCalculatedState(
    snapshotOrdinal: SnapshotOrdinal,
    state          : CheckInDataCalculatedState
  ): F[Boolean]

  def hashCalculatedState(
    state: CheckInDataCalculatedState
  ): F[Hash]
}

object CalculatedStateService {
  def make[F[_] : Async]: F[CalculatedStateService[F]] = {
    Ref.of[F, CalculatedState](CalculatedState.empty).map { stateRef =>
      new CalculatedStateService[F] {
        val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CalculatedState")

        override def getCalculatedState: F[CalculatedState] = stateRef.get

        override def setCalculatedState(
          snapshotOrdinal: SnapshotOrdinal,
          state          : CheckInDataCalculatedState
        ): F[Boolean] = {
          logger.info(s"Setting state: $snapshotOrdinal, state: $state") >>
            stateRef.update { currentState =>
              val currentVoteCalculatedState = currentState.state
              val updatedDevices = state.devices.foldLeft(currentVoteCalculatedState.devices) {
                case (acc, (address, value)) =>
                  acc.updated(address, value)
              }

              CalculatedState(snapshotOrdinal, CheckInDataCalculatedState(updatedDevices))
            }.as(true)
        }

        override def hashCalculatedState(
          state: CheckInDataCalculatedState
        ): F[Hash] = {
          val jsonState = state.asJson.deepDropNullValues.noSpaces
          logger.info(s"Hashing calculated state. Received state: $state") >>
          logger.info(s"StateToBeHashed: $jsonState") >>
            Hash.fromBytes(jsonState.getBytes(StandardCharsets.UTF_8)).pure[F]
        }
      }
    }
  }
}
