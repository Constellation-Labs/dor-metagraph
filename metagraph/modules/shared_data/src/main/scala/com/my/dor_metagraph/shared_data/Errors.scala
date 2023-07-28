package com.my.dor_metagraph.shared_data

import org.tessellation.currency.dataApplication.DataApplicationValidationError

object Errors {
  case object RepeatedCheckIn extends DataApplicationValidationError {
    val message = "Repeated Check In"
  }
}
