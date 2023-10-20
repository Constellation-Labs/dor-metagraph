package com.my.dor_metagraph.shared_data.validations

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Errors.{CheckInOlderThanAllowed, RepeatedCheckIn}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate, MINIMUM_CHECK_IN_TIMESTAMP}
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object TypeValidators {
  def validateCheckInTimestampIsGreaterThanLastCheckIn(oldState: CheckInDataCalculatedState, checkInUpdate: CheckInUpdate, address: Address): DataApplicationValidationErrorOr[Unit] = {
    val maybeState = oldState.devices.get(address)
    maybeState.map { state =>
      if (checkInUpdate.dts > state.lastCheckIn)
        ().validNec
      else
        RepeatedCheckIn.asInstanceOf[DataApplicationValidationError].invalidNec
    }.getOrElse(().validNec)
  }

  def validateCheckInLimitTimestamp(checkInUpdate: CheckInUpdate): DataApplicationValidationErrorOr[Unit] = {
    if (checkInUpdate.dts >= MINIMUM_CHECK_IN_TIMESTAMP)
      ().validNec
    else
      CheckInOlderThanAllowed.asInstanceOf[DataApplicationValidationError].invalidNec
  }
}
