package com.my.dor_metagraph.shared_data

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.my.dor_metagraph.shared_data.Data.{DeviceCheckInInfo, DeviceCheckInWithSignature, State}
import com.my.dor_metagraph.shared_data.Errors.RepeatedCheckIn
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object TypeValidators {
  def validateCheckInTimestamp(oldState: State, checkInInfo: DeviceCheckInInfo, address: Address): DataApplicationValidationErrorOr[Unit] = {
    val maybeState = oldState.devices.get(address)
    maybeState.map { state =>
      if (checkInInfo.dts > state.lastCheckIn.dts)
        ().validNec
      else
        RepeatedCheckIn.asInstanceOf[DataApplicationValidationError].invalidNec
    }.getOrElse(().validNec)
  }
}
