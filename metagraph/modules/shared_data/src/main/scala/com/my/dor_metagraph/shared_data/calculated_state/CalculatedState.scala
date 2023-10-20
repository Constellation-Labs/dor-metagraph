package com.my.dor_metagraph.shared_data.calculated_state

import cats.effect.IO
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.syntax.EncoderOps
import org.slf4j.LoggerFactory
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash

import java.security.MessageDigest

object CalculatedState {
  private val logger = LoggerFactory.getLogger("CalculatedState")

  private var maybeCheckInCalculatedState: (SnapshotOrdinal, CheckInDataCalculatedState) = (
    SnapshotOrdinal(NonNegLong(0L)),
    CheckInDataCalculatedState(Map.empty, List.empty, List.empty)
  )

  def getCalculatedState: IO[(SnapshotOrdinal, CheckInDataCalculatedState)] = {
    logger.info(s"GETTING CALCULATED STATE: $maybeCheckInCalculatedState")
    IO(maybeCheckInCalculatedState)
  }

  def setCalculatedState(snapshotOrdinal: SnapshotOrdinal, state: CheckInDataCalculatedState): IO[Boolean] = {
    val currentCheckInCalculatedState = maybeCheckInCalculatedState._2
    val updatedDevices = state.devices.foldLeft(currentCheckInCalculatedState.devices) {
      case (acc, (address, value)) =>
        acc.updated(address, value)
    }

    maybeCheckInCalculatedState = (
      snapshotOrdinal,
      CheckInDataCalculatedState(updatedDevices, state.l0ValidatorNodesAddresses, state.l1ValidatorNodesAddresses)
    )

    logger.info(s"SETTING CALCULATED STATE, NEW STATE: $maybeCheckInCalculatedState CURRENT ORDINAL: ${snapshotOrdinal}")

    IO(true)
  }

  private def sha256Hash(input: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hashBytes = digest.digest(input.getBytes("UTF-8"))
    hashBytes.map("%02x".format(_)).mkString
  }

  def hashCalculatedState(state: CheckInDataCalculatedState): IO[Hash] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    val hashedState = sha256Hash(jsonState)
    IO(Hash(hashedState))
  }
}
