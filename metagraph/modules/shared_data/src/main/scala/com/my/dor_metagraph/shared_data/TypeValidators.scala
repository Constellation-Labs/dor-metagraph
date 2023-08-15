package com.my.dor_metagraph.shared_data

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Data.{DeviceCheckInRawUpdate, State}
import com.my.dor_metagraph.shared_data.Errors.RepeatedCheckIn
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object TypeValidators {
  def validateCheckInTimestamp(oldState: State, deviceCheckInRaw: DeviceCheckInRawUpdate, address: Address): DataApplicationValidationErrorOr[Unit] = {
    val maybeState = oldState.devices.get(address)
    maybeState.map { state =>
      if (deviceCheckInRaw.dts > state.lastCheckIn.dts)
        ().validNec
      else
        RepeatedCheckIn.asInstanceOf[DataApplicationValidationError].invalidNec
    }.getOrElse(().validNec)
  }
}
