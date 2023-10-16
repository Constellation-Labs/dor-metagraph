package com.my.dor_metagraph.shared_data.serializers

import com.my.dor_metagraph.shared_data.Utils.toCBORHex
import com.my.dor_metagraph.shared_data.types.Types.{CheckInStateOnChain, CheckInUpdate}
import io.circe.syntax.EncoderOps
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock

import java.nio.charset.StandardCharsets

object Serializers {
  private val serializers: Serializers = Serializers()
  def serializeUpdate(update: CheckInUpdate): Array[Byte] = {
    serializers.customUpdateSerialization(update)
  }

  def serializeState(state: CheckInStateOnChain): Array[Byte] = {
    serializers.customStateSerialization(state)
  }

  def serializeBlock(state: DataApplicationBlock): Array[Byte] = {
    serializers.customBlockSerialization(state)
  }
}

case class Serializers(){
  private val logger = LoggerFactory.getLogger(classOf[Serializers])
  def customUpdateSerialization(update: CheckInUpdate): Array[Byte] = {
    logger.info("Serialize UPDATE event received")
    toCBORHex(update.cbor)
  }

  def customStateSerialization(state: CheckInStateOnChain): Array[Byte] = {
    logger.info("Serialize STATE event received")
    logger.info(state.asJson.deepDropNullValues.noSpaces)
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customBlockSerialization(state: DataApplicationBlock): Array[Byte] = {
    logger.info("Serialize Block event received")
    logger.info(state.asJson.deepDropNullValues.noSpaces)
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }
}