package com.my.dor_metagraph.shared_data.calculated_state

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.syntax.functor._
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import io.circe.syntax.EncoderOps
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash

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
        override def getCalculatedState: F[CalculatedState] = stateRef.get

        override def setCalculatedState(
          snapshotOrdinal: SnapshotOrdinal,
          state          : CheckInDataCalculatedState
        ): F[Boolean] =
          stateRef.update { currentState =>
            val currentVoteCalculatedState = currentState.state
            val updatedDevices = state.devices.foldLeft(currentVoteCalculatedState.devices) {
              case (acc, (address, value)) =>
                acc.updated(address, value)
            }

            CalculatedState(snapshotOrdinal, CheckInDataCalculatedState(updatedDevices))
          }.as(true)

        override def hashCalculatedState(
          state: CheckInDataCalculatedState
        ): F[Hash] = Async[F].delay {
          val jsonState = state.asJson.deepDropNullValues.noSpaces
          Hash.fromBytes(jsonState.getBytes(StandardCharsets.UTF_8))
        }
      }
    }
  }
}
