package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.Data.{DeviceCheckInRawUpdate, State}
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

object Validations {
  def deviceCheckInValidations(signedUpdate: Signed[DeviceCheckInRawUpdate], state: State)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]

    val validateTimestamp = addressIO.map { address =>
      validateCheckInTimestamp(state, signedUpdate.value, address)
    }

    validateTimestamp
  }

}
