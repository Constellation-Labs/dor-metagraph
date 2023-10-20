package com.my.dor_metagraph.shared_data.combiners

import com.my.dor_metagraph.shared_data.types.Types._
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.getNewCheckIn
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address
import org.tessellation.security.signature.Signed

object Combiners {
  private val logger = LoggerFactory.getLogger("Combiners")

  def combine(signedUpdate: Signed[CheckInUpdate], acc: DataState[CheckInStateOnChain, CheckInDataCalculatedState], address: Address, currentEpochProgress: Long): DataState[CheckInStateOnChain, CheckInDataCalculatedState] = {
    val deviceCheckIn = signedUpdate.value
    getNewCheckIn(acc, address, deviceCheckIn, currentEpochProgress)
  }

  def combineDeviceCheckIn(acc: DataState[CheckInStateOnChain, CheckInDataCalculatedState], signedUpdate: Signed[CheckInUpdate], currentEpochProgress: Long, address: Address): DataState[CheckInStateOnChain, CheckInDataCalculatedState] = {
    try {
      combine(signedUpdate, acc, address, currentEpochProgress)
    } catch {
      case e: Exception =>
        logger.warn(e.getMessage)
        logger.warn("Ignoring update and keeping with the current state")
        acc
    }
  }
}
