package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.Async
import org.tessellation.currency.dataApplication.{DataState, L0NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.combiners.Combiners.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import com.my.dor_metagraph.shared_data.validations.Validations.{deviceCheckInValidationsL0, deviceCheckInValidationsL1}
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider

object LifecycleSharedFunctions {
  private val logger = LoggerFactory.getLogger("LifecycleSharedFunctions")

  def validateUpdate[F[_]: Async](update: CheckInUpdate): F[DataApplicationValidationErrorOr[Unit]] =
     deviceCheckInValidationsL1(update)

  def validateData[F[_]: Async](oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    updates.traverse { signedUpdate =>
      deviceCheckInValidationsL0(signedUpdate.value, signedUpdate.proofs, oldState.calculated)
    }.map(_.reduce)
  }

  def combine[F[_]: Async](oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L0NodeContext[F]): F[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    val lastCurrencySnapshot = context.getLastCurrencySnapshot

    val epochProgressIO = lastCurrencySnapshot.map {
      case Some(value) => value.epochProgress.value.value
      case None =>
        val message = "Could not get the epochProgress from currency snapshot. lastCurrencySnapshot not found"
        logger.error(message)
        throw new Exception(message)
    }

    val newStateF = Async[F].delay(DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(oldState.calculated.devices)))

    if (updates.isEmpty) {
      logger.info("Snapshot without any check-ins, updating the state to empty updates")
      return newStateF
    }

    newStateF.flatMap(newState => {
      updates.foldLeftM(newState) { (acc, signedUpdate) =>
        val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[F]
        for {
          epochProgress <- epochProgressIO
          address <- addressIO
        } yield combineDeviceCheckIn(acc, signedUpdate, epochProgress + 1, address)
      }
    })
  }
}