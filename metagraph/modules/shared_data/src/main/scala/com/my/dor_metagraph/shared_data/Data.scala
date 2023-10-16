package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import org.tessellation.currency.dataApplication.{DataState, L0NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.combiners.Combiners.{combineDeviceCheckIn, getValidatorNodes}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import com.my.dor_metagraph.shared_data.validations.Validations.{deviceCheckInValidationsL0, deviceCheckInValidationsL1}
import fs2.Compiler.Target.forSync
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider

object Data {
  private val data: Data = Data()

  def validateUpdate(update: CheckInUpdate): IO[DataApplicationValidationErrorOr[Unit]] = {
    data.validateUpdate(update)
  }

  def validateData(oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: NonEmptyList[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    data.validateData(oldState, updates)
  }

  def combine(oldState: DataState[CheckInStateOnChain, CheckInDataCalculatedState], updates: List[Signed[CheckInUpdate]])(implicit context: L0NodeContext[IO]): IO[DataState[CheckInStateOnChain, CheckInDataCalculatedState]] = {
    data.combine(oldState, updates)
  }
}

case class Data() {
  private val logger = LoggerFactory.getLogger(classOf[Data])

  def validateUpdate(update: CheckInUpdate): IO[DataApplicationValidationErrorOr[Unit]] = {
    val validations =  deviceCheckInValidationsL1(update)
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
    val epochProgressIO = context.getLastCurrencySnapshot.map(_.get.epochProgress)

    val validatorNodesIO = for {
      epochProgress <- epochProgressIO
    } yield getValidatorNodes(epochProgress.value.value + 1, oldState.calculated, sp)

    val newStateIO = for {
      validatorNodes <- validatorNodesIO
      l0ValidatorNodes <- validatorNodes._1
      l1ValidatorNodes <- validatorNodes._2
    } yield DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(oldState.calculated.devices, l0ValidatorNodes, l1ValidatorNodes))

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
        } yield combineDeviceCheckIn(acc, signedUpdate, epochProgress.value.value + 1, address)
      }
    })
  }
}