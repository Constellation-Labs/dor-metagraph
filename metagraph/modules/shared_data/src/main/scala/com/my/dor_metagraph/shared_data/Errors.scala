package com.my.dor_metagraph.shared_data

import cats.syntax.validated.catsSyntaxValidatedIdBinCompat0
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

object Errors {
  private type DataApplicationValidationType = DataApplicationValidationErrorOr[Unit]

  val valid: DataApplicationValidationType = ().validNec[DataApplicationValidationError]

  implicit class DataApplicationValidationTypeOps[E <: DataApplicationValidationError](err: E) {
    def invalid: DataApplicationValidationType = err.invalidNec[Unit]

    def unlessA(cond: Boolean): DataApplicationValidationType = if (cond) valid else invalid

    def whenA(cond: Boolean): DataApplicationValidationType = if (cond) invalid else valid
  }

  case object RepeatedCheckIn extends DataApplicationValidationError {
    val message = "Repeated Check In"
  }

  case object CheckInOlderThanAllowed extends DataApplicationValidationError {
    val message = "CheckIn is older than allowed"
  }

  case object FutureCheckInNotAllowed extends DataApplicationValidationError {
    val message = "Future Check In not allowed"
  }

  case object DeviceNotRegisteredOnDorApi extends DataApplicationValidationError {
    val message = "Device not registered on Dor API"
  }
}
