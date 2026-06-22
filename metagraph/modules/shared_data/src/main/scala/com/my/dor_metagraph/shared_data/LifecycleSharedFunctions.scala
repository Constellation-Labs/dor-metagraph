package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.getFirstAddressFromProofs
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import com.my.dor_metagraph.shared_data.validations.Validations.{deviceCheckInValidationsL0, deviceCheckInValidationsL1}
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.epoch.EpochProgress._
import io.constellationnetwork.schema.epoch._
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object LifecycleSharedFunctions {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("LifecycleSharedFunctions")

  def validateUpdate[F[_] : Async](
    update: CheckInUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] =
    deviceCheckInValidationsL1(update)

  def validateData[F[_] : Async](
    oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState],
    updates : NonEmptyList[Signed[CheckInUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    updates.traverse { signedUpdate =>
      deviceCheckInValidationsL0(signedUpdate.value, signedUpdate.proofs, oldState.calculated)
    }.map(_.reduce)
  }

  /**
    * Epoch progress to use when combining check-ins. Prefers the live context; falls back to
    * the value persisted in the calculated state when the context has no snapshot yet (e.g.
    * during a rollback replay on framework versions that do not seed the snapshot storage
    * before DataApplicationTraverse runs). The stored value is the one used by the previous
    * non-empty combine, so the fallback reproduces the original computation exactly whenever
    * the epoch did not advance in between, and stays within the elapsed-epoch gap otherwise.
    */
  private[shared_data] def getCurrentEpochProgress[F[_] : Async](
    oldCalculatedState: CheckInDataCalculatedState
  )(implicit context: L0NodeContext[F]): F[EpochProgress] =
    context.getLastCurrencySnapshot.flatMap {
      case Some(value) => value.epochProgress.next.pure[F]
      case None =>
        oldCalculatedState.lastEpochProgress match {
          case Some(storedEpochProgress) =>
            logger.warn(
              s"lastCurrencySnapshot unavailable, falling back to lastEpochProgress=${storedEpochProgress.value.value} from calculated state"
            ).as(storedEpochProgress)
          case None =>
            val message = "Could not get the epochProgress from currency snapshot. lastCurrencySnapshot not found"
            logger.error(message) >> new Exception(message).raiseError[F, EpochProgress]
        }
    }

  def combine[F[_] : Async](
    oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState],
    updates : List[Signed[CheckInUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    // The framework seeds each snapshot's block fold with the previous snapshot's state, so
    // onChain must reset here. With several blocks in one snapshot only the last combine call's
    // updates end up in the published onChain state; the full check-in list is still available
    // through the snapshot's data blocks.
    val newState = DataState(
      CheckInStateOnChain(List.empty),
      CheckInDataCalculatedState(oldState.calculated.devices, oldState.calculated.lastEpochProgress)
    )

    if (updates.isEmpty) {
      newState.pure[F]
    } else {
      for {
        nextEpoch <- getCurrentEpochProgress(oldState.calculated)
        result <- updates.foldLeftM(newState) { (acc, signedUpdate) =>
          getFirstAddressFromProofs(signedUpdate.proofs)
            .map(address => combineDeviceCheckIn(acc, signedUpdate, address, nextEpoch))
        }
      } yield result.copy(calculated = result.calculated.copy(lastEpochProgress = nextEpoch.some))
    }
  }
}