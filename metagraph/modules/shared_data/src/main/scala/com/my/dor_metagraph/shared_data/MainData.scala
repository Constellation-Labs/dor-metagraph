package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.tessellation.currency.dataApplication.{DataState, DataUpdate, L0NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import Combiners.combineDeviceCheckin
import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs._
import org.tessellation.schema.address.Address

object MainData {
  implicit val dorCodec: Codec[DeviceCheckin] = deriveCodec[DeviceCheckin]

  @derive(decoder, encoder)
  case class DeviceCheckin(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class DeviceCheckinWithEpochProgress(ac: List[Long], dts: Long, e: List[List[Long]], epochProgress: Long)

  @derive(decoder, encoder)
  case class DeviceUpdate(data: String) extends DataUpdate

  @derive(decoder, encoder)
  case class State(devices: Map[Address, DeviceCheckinWithEpochProgress]) extends DataState

  def combine(oldState: State, updates: NonEmptyList[Signed[DeviceUpdate]])(implicit context: L0NodeContext[IO]): IO[State] = {
    updates.foldLeftM(oldState) { (acc, signedUpdate) =>
      combineDeviceCheckin(acc, signedUpdate)
    }
  }

  def serializeState(state: State): IO[Array[Byte]] = IO {
    Utils.customStateSerialization(state)
  }

  def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, State]] = IO {
    Utils.customStateDeserialization(bytes)
  }

  def serializeUpdate(update: DeviceUpdate): IO[Array[Byte]] = IO {
    Utils.customUpdateSerialization(update)
  }

  def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceUpdate]] = IO {
    Utils.customUpdateDeserialization(bytes)
  }

  def dataEncoder: Encoder[DeviceUpdate] = deriveEncoder

  def dataDecoder: Decoder[DeviceUpdate] = deriveDecoder
}