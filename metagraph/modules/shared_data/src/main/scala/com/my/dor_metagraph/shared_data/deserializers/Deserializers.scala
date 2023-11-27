package com.my.dor_metagraph.shared_data.deserializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, CheckInStateOnChain, CheckInUpdate}
import io.circe.{Decoder, parser}
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed

import java.nio.charset.StandardCharsets

object Deserializers {

  def deserializeUpdate(bytes: Array[Byte]): Either[Throwable, CheckInUpdate] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[CheckInUpdate]
    }
  }

  def deserializeState(bytes: Array[Byte]): Either[Throwable, CheckInStateOnChain] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[CheckInStateOnChain]
    }
  }

  def deserializeBlock(bytes: Array[Byte])(implicit e: Decoder[DataUpdate]): Either[Throwable, Signed[DataApplicationBlock]] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[Signed[DataApplicationBlock]]
    }
  }

  def deserializeCalculatedState(bytes: Array[Byte]): Either[Throwable, CheckInDataCalculatedState] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[CheckInDataCalculatedState]
    }
  }
}