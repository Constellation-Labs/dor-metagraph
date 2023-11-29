package com.my.dor_metagraph.shared_data.validations

import com.my.dor_metagraph.shared_data.Errors.{CheckInOlderThanAllowed, DeviceNotRegisteredOnDorApi, RepeatedCheckIn}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate, MinimumCheckInSeconds}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object TypeValidators {

  def validateCheckInTimestampIsGreaterThanLastCheckIn(
    state        : CheckInDataCalculatedState,
    checkInUpdate: CheckInUpdate,
    address      : Address
  ): DataApplicationValidationErrorOr[Unit] =
    RepeatedCheckIn.whenA(state.devices.get(address).exists(_.lastCheckIn >= checkInUpdate.dts))

  def validateCheckInLimitTimestamp(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    CheckInOlderThanAllowed.whenA(MinimumCheckInSeconds > checkInUpdate.dts)

  def validateIfDeviceIsRegisteredOnDORApi(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    DeviceNotRegisteredOnDorApi.unlessA(checkInUpdate.maybeDorAPIResponse.isDefined)
}
