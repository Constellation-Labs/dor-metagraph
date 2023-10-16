package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.validations.TypeValidators.{validateCheckInLimitTimestamp, validateCheckInTimestampIsGreaterThanLastCheckIn}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object Validations {
  def deviceCheckInValidationsL0(checkInUpdate: CheckInUpdate, proofs: NonEmptySet[SignatureProof], state: CheckInDataCalculatedState)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val addressIO = proofs.map(_.id).head.toAddress[IO]

    val validateCurrentCheckInGreaterThanLast = addressIO.map { address =>
      validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInUpdate, address)
    }

    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate).pure[IO]

    validateCurrentCheckInGreaterThanLast.productR(validateCheckInLimit)
  }

  def deviceCheckInValidationsL1(checkInUpdate: CheckInUpdate): IO[DataApplicationValidationErrorOr[Unit]] = {
    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate).pure[IO]
    validateCheckInLimit
  }

}
