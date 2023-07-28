package com.my.dor_metagraph.shared_data

import Data.{DeviceUpdate, State}
import io.circe.parser
import io.circe.syntax.EncoderOps

import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

object Utils {
  def toCBORHex(hexString: String): Array[Byte] = {
    try {
      if ((hexString.length & 1) != 0) sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    } catch {
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
    }
  }

  def customUpdateSerialization(update: DeviceUpdate): Array[Byte] = {
    println("Serialize UPDATE event received")
    println(update.asJson.deepDropNullValues.noSpaces)
    update.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customStateSerialization(state: State): Array[Byte] = {
    println("Serialize STATE event received")
    println(state.asJson.deepDropNullValues.noSpaces)
    state.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
  }

  def customStateDeserialization(bytes: Array[Byte]): Either[Throwable, State] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[State]
    }
  }

  def customUpdateDeserialization(bytes: Array[Byte]): Either[Throwable, DeviceUpdate] = {
    parser.parse(new String(bytes, StandardCharsets.UTF_8)).flatMap { json =>
      json.as[DeviceUpdate]
    }
  }

  def toTokenAmountFormat(balance: Double): Long = {
    (balance * 10e7).toLong
  }
}
