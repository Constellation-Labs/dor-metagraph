package com.my.dor_metagraph.shared_data.deserializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, parser}
import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationBlock
import io.constellationnetwork.security.signature.Signed

import java.nio.charset.StandardCharsets

object Deserializers {

  private def deserialize[A: Decoder](
    bytes: Array[Byte]
  ): Either[Throwable, A] =
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[A]
    }

  def deserializeUpdate(
    bytes: Array[Byte]
  ): Either[Throwable, CheckInUpdate] =
    deserialize[CheckInUpdate](bytes)

  def deserializeState(
    bytes: Array[Byte]
  ): Either[Throwable, CheckInStateOnChain] =
    deserialize[CheckInStateOnChain](bytes)

  def deserializeBlock(
    bytes: Array[Byte]
  )(implicit e: Decoder[DataUpdate]): Either[Throwable, Signed[DataApplicationBlock]] =
    deserialize[Signed[DataApplicationBlock]](bytes)

  def deserializeCalculatedState(
    bytes: Array[Byte]
  ): Either[Throwable, CheckInDataCalculatedState] =
    deserialize[CheckInDataCalculatedState](bytes)
}