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
    // NOTE: validateIfCheckInIsLowerThanOneDayFromCurrentDate is intentionally NOT applied here.
    // It reads Instant.now (wall clock), and this path runs inside validateData during L0 consensus
    // — every validator must reach the SAME verdict for the same update, but wall clocks differ
    // across nodes, so a check-in near the now+1day boundary could be accepted by some validators
    // and rejected by others, diverging the combine input set and forking the calculated-state hash.
    // The future-dated-check-in guard is kept on the non-consensus L1 ingress path (validateUpdate)
    // below, where per-node wall-clock use is harmless.
    getFirstAddressFromProofs(proofs).map { address =>
      validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInUpdate, address)
        .productR(validateIfCheckInIsGreaterThanLimitTimestamp(checkInUpdate))
        .productR(validateIfDeviceIsRegisteredOnDORApi(checkInUpdate))
        .productR(validateExactlyOneProof(proofs))
        .productR(validateBilledAmountWithinBounds(checkInUpdate))
        .productR(validateCheckInHashIsNotRepeated(state, checkInUpdate, address))
    }


  def deviceCheckInValidationsL1[F[_] : Async](
    checkInUpdate: CheckInUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    validateIfCheckInIsGreaterThanLimitTimestamp(checkInUpdate)
      .productR(validateIfCheckInIsLowerThanOneDayFromCurrentDate(checkInUpdate))
      .productR(validateIfDeviceIsRegisteredOnDORApi(checkInUpdate))
      .productR(validateBilledAmountWithinBounds(checkInUpdate))
  }

}
