package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import org.tessellation.currency.dataApplication.{DataState, L0NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.combiners.Combiners.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import com.my.dor_metagraph.shared_data.validations.Validations.{deviceCheckInValidationsL0, deviceCheckInValidationsL1}
import fs2.Compiler.Target.forSync
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider

object Data {
  private val logger = LoggerFactory.getLogger("Data")

  def validateUpdate(update: CheckInUpdate): IO[DataApplicationValidationErrorOr[Unit]] = {
    val validations = deviceCheckInValidationsL1(update)
    validations
  }

  def validateData(oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    updates.traverse { signedUpdate =>
      deviceCheckInValidationsL0(signedUpdate.value, signedUpdate.proofs, oldState.calculated)
    }.map(_.reduce)
  }

  def combine(oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    val lastCurrencySnapshot = context.getLastCurrencySnapshot

    val epochProgressIO = lastCurrencySnapshot.map {
      case Some(value) => value.epochProgress.value.value
      case None =>
        val message = "Could not get the epochProgress from currency snapshot. lastCurrencySnapshot not found"
        logger.error(message)
        throw new Exception(message)
    }

    val newStateIO = IO(DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(oldState.calculated.devices)))

    if (updates.isEmpty) {
      logger.info("Snapshot without any check-ins, updating the state to empty updates")
      return newStateIO
    }

    newStateIO.flatMap(newState => {
      updates.foldLeftM(newState) { (acc, signedUpdate) =>
        val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]
        for {
          epochProgress <- epochProgressIO
          address <- addressIO
        } yield combineDeviceCheckIn(acc, signedUpdate, epochProgress + 1, address)
      }
    })
  }
}