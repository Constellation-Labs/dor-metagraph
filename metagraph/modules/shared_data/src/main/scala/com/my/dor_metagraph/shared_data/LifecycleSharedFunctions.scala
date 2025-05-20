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

  def combine[F[_] : Async](
    oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState],
    updates : List[Signed[CheckInUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    val newState = DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(oldState.calculated.devices))

    if (updates.isEmpty) {
      logger.info("Snapshot without any check-ins, updating the state to empty updates").as(newState)
    } else {
      updates.foldLeftM(newState) { (acc, signedUpdate) =>
        for {
          epochProgress <- context.getLastCurrencySnapshot.flatMap {
            case Some(value) => value.epochProgress.pure[F]
            case None =>
              val message = "Could not get the epochProgress from currency snapshot. lastCurrencySnapshot not found"
              logger.error(message) >> new Exception(message).raiseError[F, EpochProgress]
          }
          address <- getFirstAddressFromProofs(signedUpdate.proofs)
          _ <- logger.info(s"New checkIn for the device: $signedUpdate")
        } yield combineDeviceCheckIn(acc, signedUpdate, address, epochProgress.next)
      }
    }
  }
}