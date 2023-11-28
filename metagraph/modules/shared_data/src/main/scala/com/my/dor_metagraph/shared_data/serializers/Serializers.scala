package com.my.dor_metagraph.shared_data.serializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed

import java.nio.charset.StandardCharsets

object Serializers {
  def serializeUpdate(update: CheckInUpdate): Array[Byte] = {
    update.dtmCheckInHash.getBytes(StandardCharsets.UTF_8)
  }

  def serializeState(state: CheckInStateOnChain): Array[Byte] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    jsonState.getBytes(StandardCharsets.UTF_8)
  }

  def serializeBlock(state: Signed[DataApplicationBlock])(implicit e: Encoder[DataUpdate]): Array[Byte] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def serializeCalculatedState(state: CheckInDataCalculatedState): Array[Byte] = {
    val jsonState = state.asJson.deepDropNullValues.noSpaces
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }
}