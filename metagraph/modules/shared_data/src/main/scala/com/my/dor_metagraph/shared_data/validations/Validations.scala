package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApply
import com.my.dor_metagraph.shared_data.validations.TypeValidators.{validateCheckInLimitTimestamp, validateCheckInTimestampIsGreaterThanLastCheckIn, validateIfDeviceIsRegisteredOnDORApi}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInUpdate}
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object Validations {
  private val logger = LoggerFactory.getLogger("Validations")
  def deviceCheckInValidationsL0(checkInUpdate: CheckInUpdate, proofs: NonEmptySet[SignatureProof], state: CheckInDataCalculatedState)(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    val address = proofs.map(_.id).head.toAddress[IO].unsafeRunSync()
    val validateCurrentCheckInGreaterThanLast = validateCheckInTimestampIsGreaterThanLastCheckIn(state, checkInUpdate, address)
    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate)
    val validateIfDeviceDORApi = validateIfDeviceIsRegisteredOnDORApi(checkInUpdate)

    IO {
      validateCurrentCheckInGreaterThanLast.productR(validateCheckInLimit).productR(validateIfDeviceDORApi)
    }
  }

  def deviceCheckInValidationsL1(checkInUpdate: CheckInUpdate): IO[DataApplicationValidationErrorOr[Unit]] = {
    val validateCheckInLimit = validateCheckInLimitTimestamp(checkInUpdate)
    val validateIfDeviceDORApi = validateIfDeviceIsRegisteredOnDORApi(checkInUpdate)

    IO {
      validateCheckInLimit.productR(validateIfDeviceDORApi)
    }
  }

}
