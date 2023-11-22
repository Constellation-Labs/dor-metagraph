package com.my.dor_metagraph.shared_data.calculated_state

import cats.Applicative
import cats.effect.kernel.Async
import cats.implicits.{catsSyntaxApplicativeId, toFunctorOps}
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.syntax.EncoderOps
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

object CalculatedState {

  private val stateRef: AtomicReference[(SnapshotOrdinal, CheckInDataCalculatedState)] = new AtomicReference(
    (SnapshotOrdinal(NonNegLong(0L)),
      CheckInDataCalculatedState(Map.empty))
  )

  def getCalculatedState[F[_] : Applicative]: F[(SnapshotOrdinal, CheckInDataCalculatedState)] = {
    stateRef.get().pure[F]
  }

  def setCalculatedState[F[_] : Async](snapshotOrdinal: SnapshotOrdinal, state: CheckInDataCalculatedState): F[Boolean] = Async[F].delay {
    val currentCheckInCalculatedState = stateRef.get()._2
    val updatedDevices = state.devices.foldLeft(currentCheckInCalculatedState.devices) {
      case (acc, (address, value)) =>
        acc.updated(address, value)
    }

    stateRef.set((
      snapshotOrdinal,
      CheckInDataCalculatedState(updatedDevices)
    ))
  }.as(true)

  def hashCalculatedState[F[_] : Async](state: CheckInDataCalculatedState): F[Hash] = Async[F].delay {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    Hash.fromBytes(jsonState.getBytes(StandardCharsets.UTF_8))
  }
}
