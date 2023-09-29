package com.my.dor_metagraph.shared_data

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxValidatedIdBinCompat0}
import com.my.dor_metagraph.shared_data.Errors.{CouldNotGetLatestCurrencySnapshot, CouldNotGetLatestState}
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.TypeValidators.{validateCheckInLimitTimestamp, validateCheckInTimestampIsGreaterThanLastCheckIn}
import com.my.dor_metagraph.shared_data.Utils.customStateDeserialization
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address
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

  def deviceCheckInValidationsL1(checkInInfo: DeviceCheckInInfo, maybeStateRaw: Option[Array[Byte]], address: Address): IO[DataApplicationValidationErrorOr[Unit]] = {
    maybeStateRaw match {
      case None => CouldNotGetLatestCurrencySnapshot.asInstanceOf[DataApplicationValidationError].invalidNec.pure[IO]
      case Some(value) =>
        val currentState = customStateDeserialization(value)
        currentState match {
          case Left(_) => CouldNotGetLatestState.asInstanceOf[DataApplicationValidationError].invalidNec.pure[IO]
          case Right(state) =>
            val validateCurrentCheckInGreaterThanLast = validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInInfo, address).pure[IO]
            val validateCheckInLimit = validateCheckInLimitTimestamp(checkInInfo).pure[IO]

            validateCurrentCheckInGreaterThanLast.productR(validateCheckInLimit)
        }
    }
  }

}
