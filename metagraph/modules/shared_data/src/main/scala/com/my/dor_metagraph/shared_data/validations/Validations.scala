package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.apply._
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.shared_data.Utils.getFirstAddressFromProofs
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate}
import com.my.dor_metagraph.shared_data.validations.TypeValidators._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.signature.SignatureProof

object Validations {
  def deviceCheckInValidationsL0[F[_] : Async](
    checkInUpdate: CheckInUpdate,
    proofs       : NonEmptySet[SignatureProof],
    state        : CheckInDataCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] =
    getFirstAddressFromProofs(proofs).map { address =>
      validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInUpdate, address)
        .productR(validateIfCheckInIsGreaterThanLimitTimestamp(checkInUpdate))
        .productR(validateIfCheckInIsLowerThanOneDayFromCurrentDate(checkInUpdate))
        .productR(validateIfDeviceIsRegisteredOnDORApi(checkInUpdate))
    }


  def deviceCheckInValidationsL1[F[_] : Async](
    checkInUpdate: CheckInUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    validateIfCheckInIsGreaterThanLimitTimestamp(checkInUpdate)
      .productR(validateIfCheckInIsLowerThanOneDayFromCurrentDate(checkInUpdate))
      .productR(validateIfDeviceIsRegisteredOnDORApi(checkInUpdate))
  }

}
