package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import com.my.dor_metagraph.shared_data.Errors._
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate, MaxBilledAmount, MinimumCheckInSeconds}
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.signature.signature.SignatureProof

import java.time.Instant
import java.time.temporal.ChronoUnit

object TypeValidators {

  def validateExactlyOneProof(
    proofs: NonEmptySet[SignatureProof]
  ): DataApplicationValidationErrorOr[Unit] =
    MultipleProofsNotAllowed.whenA(proofs.toSortedSet.size > 1)

  def validateBilledAmountWithinBounds(
    checkInUpdate: CheckInUpdate
  ): DataApplicationValidationErrorOr[Unit] = {
    val amounts = checkInUpdate.maybeDorAPIResponse.toList.flatMap { response =>
      response.billedAmount.toList ::: response.billedAmountMonthly.toList
    }
    BilledAmountOutOfBounds.whenA(amounts.exists(amount => amount < 0L || amount > MaxBilledAmount))
  }

  def validateCheckInHashIsNotRepeated(
    state        : CheckInDataCalculatedState,
    checkInUpdate: CheckInUpdate,
    address      : Address
  ): DataApplicationValidationErrorOr[Unit] =
    RepeatedCheckInHash.whenA(
      state.devices.get(address).flatMap(_.lastCheckInHash).contains(checkInUpdate.dtmCheckInHash)
    )

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
