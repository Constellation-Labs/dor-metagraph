package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.Data.{DeviceCheckInRaw, DeviceUpdate, State}
import com.my.dor_metagraph.shared_data.TypeValidators.validateCheckInTimestamp
import io.bullet.borer.Cbor
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

object Validations {
  def deviceCheckInValidations(signedUpdate: Signed[DeviceUpdate], state: State)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val cborData = Utils.toCBORHex(signedUpdate.value.data)
    val deviceCheckIn: DeviceCheckInRaw = Cbor.decode(cborData).to[DeviceCheckInRaw].value
    val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]

    val validateTimestamp = addressIO.map { address =>
      validateCheckInTimestamp(state, deviceCheckIn, address)
    }

    validateTimestamp
  }

}
