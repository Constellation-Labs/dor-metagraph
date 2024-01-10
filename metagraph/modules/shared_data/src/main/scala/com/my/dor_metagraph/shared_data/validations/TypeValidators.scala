package com.my.dor_metagraph.shared_data.validations

import com.my.dor_metagraph.shared_data.Errors._
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate, MinimumCheckInSeconds}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

import java.time.Instant
import java.time.temporal.ChronoUnit

object TypeValidators {

  def validateCheckInTimestampIsGreaterThanLastCheckIn(
    state        : CheckInDataCalculatedState,
    checkInUpdate: CheckInUpdate,
    address      : Address
  ): DataApplicationValidationErrorOr[Unit] =
    RepeatedCheckIn.whenA(state.devices.get(address).exists(_.lastCheckIn >= checkInUpdate.dts))

  def validateIfCheckInIsGreaterThanLimitTimestamp(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    CheckInOlderThanAllowed.whenA(MinimumCheckInSeconds > checkInUpdate.dts)

  def validateIfCheckInIsLowerThanOneDayFromCurrentDate(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    FutureCheckInNotAllowed.whenA(Instant.now.plus(1, ChronoUnit.DAYS).toEpochMilli / 1000L < checkInUpdate.dts)

  def validateIfDeviceIsRegisteredOnDORApi(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    DeviceNotRegisteredOnDorApi.unlessA(checkInUpdate.maybeDorAPIResponse.isDefined)
}
