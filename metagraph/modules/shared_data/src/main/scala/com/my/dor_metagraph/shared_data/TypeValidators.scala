package com.my.dor_metagraph.shared_data

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInInfo, MINIMUM_CHECK_IN_TIMESTAMP}
import com.my.dor_metagraph.shared_data.Errors.{CheckInOlderThanAllowed, RepeatedCheckIn}
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object TypeValidators {
  def validateCheckInTimestampIsGreaterThanLastCheckIn(oldState: CheckInState, checkInInfo: DeviceCheckInInfo, address: Address): DataApplicationValidationErrorOr[Unit] = {
    val maybeState = oldState.devices.get(address)
    maybeState.map { state =>
      if (checkInInfo.dts > state.lastCheckIn)
        ().validNec
      else
        RepeatedCheckIn.asInstanceOf[DataApplicationValidationError].invalidNec
    }.getOrElse(().validNec)
  }

  def validateCheckInLimitTimestamp(checkInInfo: DeviceCheckInInfo): DataApplicationValidationErrorOr[Unit] = {
    if (checkInInfo.dts >= MINIMUM_CHECK_IN_TIMESTAMP)
      ().validNec
    else
      CheckInOlderThanAllowed.asInstanceOf[DataApplicationValidationError].invalidNec
  }
}
