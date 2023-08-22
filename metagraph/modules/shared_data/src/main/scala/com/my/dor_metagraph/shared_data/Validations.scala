package com.my.dor_metagraph.shared_data

import cats.data.NonEmptySet
import cats.effect.IO
import com.my.dor_metagraph.shared_data.Types.{DeviceCheckInInfo, CheckInState}
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object Validations {
  def deviceCheckInValidations(checkInInfo: DeviceCheckInInfo, proofs: NonEmptySet[SignatureProof], state: CheckInState)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val addressIO = proofs.map(_.id).head.toAddress[IO]

    val validateTimestamp = addressIO.map { address =>
      validateCheckInTimestamp(state, checkInInfo, address)
    }

    validateTimestamp
  }

}
