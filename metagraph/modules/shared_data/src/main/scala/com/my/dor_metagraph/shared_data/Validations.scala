package com.my.dor_metagraph.shared_data

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.TypeValidators.{validateCheckInLimitTimestamp, validateCheckInTimestampIsGreaterThanLastCheckIn}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object Validations {
  def deviceCheckInValidationsL0(checkInInfo: DeviceCheckInInfo, proofs: NonEmptySet[SignatureProof], state: CheckInState)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val addressIO = proofs.map(_.id).head.toAddress[IO]

    val validateCurrentCheckInGreaterThanLast = addressIO.map { address =>
      validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInInfo, address)
    }

    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInInfo).pure[IO]

    validateCurrentCheckInGreaterThanLast.productR(validateCheckInLimit)
  }

  def deviceCheckInValidationsL1(checkInInfo: DeviceCheckInInfo): IO[DataApplicationValidationErrorOr[Unit]] = {
    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInInfo).pure[IO]
    validateCheckInLimit
  }

}
