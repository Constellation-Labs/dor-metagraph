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
import Combiners.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.Bounties.Bounty
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import com.my.dor_metagraph.shared_data.Validations.deviceCheckInValidations
import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs._
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

object Data {
  implicit val dorCodec: Codec[DeviceCheckInRaw] = deriveCodec[DeviceCheckInRaw]

  @derive(decoder, encoder)
  case class DeviceCheckInRaw(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class FootTraffic(timestamp: Long, direction: Long)

  @derive(decoder, encoder)
  case class CheckInRef(ordinal: Long, hash: String)

  @derive(decoder, encoder)
  case class DeviceCheckInFormatted(ac: List[Long], dts: Long, footTraffics: List[FootTraffic], checkInRef: CheckInRef)

  @derive(decoder, encoder)
  case class DeviceInfo(lastCheckIn: DeviceCheckInFormatted, publicKey: String, bounties: List[Bounty], deviceApiResponse: DeviceInfoAPIResponse, lastCheckInEpochProgress: Long)

  @derive(decoder, encoder)
  case class DeviceUpdate(data: String) extends DataUpdate

  @derive(decoder, encoder)
  case class State(devices: Map[Address, DeviceInfo]) extends DataState

  def validateData(oldState: State, updates: NonEmptyList[Signed[DeviceUpdate]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    updates.traverse { signedUpdate =>
      deviceCheckInValidations(signedUpdate, oldState)
    }.map(_.reduce)
  }


  def combine(oldState: State, updates: NonEmptyList[Signed[DeviceUpdate]])(implicit context: L0NodeContext[IO]): IO[State] = {
    updates.foldLeftM(oldState) { (acc, signedUpdate) =>
      combineDeviceCheckIn(acc, signedUpdate)
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