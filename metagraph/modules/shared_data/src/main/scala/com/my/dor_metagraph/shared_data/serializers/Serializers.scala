package com.my.dor_metagraph.shared_data.serializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInStateOnChain, CheckInUpdate}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed
import java.nio.charset.StandardCharsets

object Serializers {
  private val logger = LoggerFactory.getLogger("Serializers")
  def serializeUpdate(update: CheckInUpdate): Array[Byte] = {
    logger.info(s"Serialize UPDATE event received ${update.dtmCheckInHash}")
    update.dtmCheckInHash.getBytes(StandardCharsets.UTF_8)
  }

  def serializeState(state: CheckInStateOnChain): Array[Byte] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    logger.info(s"Serialize STATE event received: $jsonState")
    jsonState.getBytes(StandardCharsets.UTF_8)
  }

  def serializeBlock(state: Signed[DataApplicationBlock])(implicit e: Encoder[DataUpdate]): Array[Byte] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    logger.info(s"Serialize BLOCK event received: $jsonState")
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }
}