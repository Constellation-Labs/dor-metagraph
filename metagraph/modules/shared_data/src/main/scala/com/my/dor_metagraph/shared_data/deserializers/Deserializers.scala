package com.my.dor_metagraph.shared_data.deserializers

import com.my.dor_metagraph.shared_data.types.Types.{CheckInStateOnChain, CheckInUpdate}
import io.circe.parser
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock

import java.nio.charset.StandardCharsets

object Deserializers {
  private val deserializers: Deserializers = Deserializers()
  def deserializeUpdate(bytes: Array[Byte]): Either[Throwable, CheckInUpdate] = {
    deserializers.customUpdateDeserialization(bytes)
  }

  def deserializeState(bytes: Array[Byte]): Either[Throwable, CheckInStateOnChain] = {
    deserializers.customStateDeserialization(bytes)
  }

  def deserializeBlock(bytes: Array[Byte]): Either[Throwable, DataApplicationBlock] = {
    deserializers.customBlockDeserialization(bytes)
  }
}

case class Deserializers(){
  private val logger = LoggerFactory.getLogger(classOf[Deserializers])

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, CheckInUpdate] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      logger.info(json.toString())
      json.as[CheckInUpdate]
    }
  }

  def customStateDeserialization(bytes: Array[Byte]): Either[Throwable, CheckInStateOnChain] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[CheckInStateOnChain]
    }
  }

  def customBlockDeserialization(bytes: Array[Byte]): Either[Throwable, DataApplicationBlock] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[DataApplicationBlock]
    }
  }
}