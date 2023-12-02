package com.my.dor_metagraph.shared_data.serializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed

import java.nio.charset.StandardCharsets

object Serializers {
  private def serialize[A: Encoder](
    serializableData: A
  ): Array[Byte] =
    serializableData.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)

  def serializeUpdate(
    update: CheckInUpdate
  ): Array[Byte] =
    update.dtmCheckInHash.getBytes(StandardCharsets.UTF_8)

  def serializeState(
    state: CheckInStateOnChain
  ): Array[Byte] =
    serialize[CheckInStateOnChain](state)

  def serializeBlock(
    block: Signed[DataApplicationBlock]
  )(implicit e: Encoder[DataUpdate]): Array[Byte] =
    serialize[Signed[DataApplicationBlock]](block)

  def serializeCalculatedState(
    calculatedState: CheckInDataCalculatedState
  ): Array[Byte] =
    serialize[CheckInDataCalculatedState](calculatedState)
}