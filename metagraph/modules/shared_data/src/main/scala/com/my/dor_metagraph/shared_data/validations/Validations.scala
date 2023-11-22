package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits.{catsSyntaxApply, toFunctorOps}
import com.my.dor_metagraph.shared_data.validations.TypeValidators._
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof
import com.my.dor_metagraph.shared_data.Utils.getFirstAddressFromProofs

object Validations {
  def deviceCheckInValidationsL0[F[_] : Async](checkInUpdate: CheckInUpdate, proofs: NonEmptySet[SignatureProof], state: CheckInDataCalculatedState)(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] =
    for {
      address <- getFirstAddressFromProofs(proofs)
      validatedAddress = validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInUpdate, address)
      validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate)
      validateIfDeviceDORApi = validateIfDeviceIsRegisteredOnDORApi(checkInUpdate)
    } yield validatedAddress.productR(validateCheckInLimit).productR(validateIfDeviceDORApi)


  def deviceCheckInValidationsL1[F[_] : Async](checkInUpdate: CheckInUpdate): F[DataApplicationValidationErrorOr[Unit]] = {
    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate)
    val validateIfDeviceDORApi = validateIfDeviceIsRegisteredOnDORApi(checkInUpdate)
    Async[F].delay(validateCheckInLimit.productR(validateIfDeviceDORApi))
  }

}
