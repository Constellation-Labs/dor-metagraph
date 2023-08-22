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
import com.my.dor_metagraph.shared_data.Utils.{buildSignedUpdate, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.Validations.deviceCheckInValidations
import fs2.Compiler.Target.forSync
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

object Data {
  @derive(decoder, encoder)
  case class FootTraffic(timestamp: Long, direction: Long)

  @derive(decoder, encoder)
  case class CheckInRef(ordinal: Long, hash: String)

  @derive(decoder, encoder)
  case class DeviceCheckInFormatted(ac: List[Long], dts: Long, footTraffics: List[FootTraffic], checkInRef: CheckInRef)

  @derive(decoder, encoder)
  case class DeviceInfo(lastCheckIn: DeviceCheckInFormatted, publicKey: String, bounties: List[Bounty], deviceApiResponse: DeviceInfoAPIResponse, lastCheckInEpochProgress: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInWithSignature(cbor: String, id: String, sig: String) extends DataUpdate

  @derive(decoder, encoder)
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class State(devices: Map[Address, DeviceInfo]) extends DataState

  def validateData(oldState: State, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    updates.traverse { signedUpdate =>
      val checkInInfo = getDeviceCheckInInfo(signedUpdate.value.cbor)
      deviceCheckInValidations(checkInInfo, signedUpdate.proofs, oldState)
    }.map(_.reduce)
  }


  def combine(oldState: State, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[State] = {
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

  def serializeUpdate(update: DeviceCheckInWithSignature): IO[Array[Byte]] = IO {
    Utils.customUpdateSerialization(update)
  }

  def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceCheckInWithSignature]] = IO {
    Utils.customUpdateDeserialization(bytes)
  }

  def dataEncoder: Encoder[DeviceCheckInWithSignature] = deriveEncoder

  def dataDecoder: Decoder[DeviceCheckInWithSignature] = deriveDecoder

  def signedDataEntityDecoder: EntityDecoder[IO, Signed[DeviceCheckInWithSignature]] = {
    EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
      val rawText = msg.as[String]
      val signed = rawText.flatMap { text =>
        val byteArray = if (text.contains(',')) {
          text.split(',').map(byteAsString => Integer.decode(byteAsString.trim).toByte)
        } else {
          text.split(' ').map(byteAsString => Integer.decode(byteAsString.trim).toByte)
        }
        buildSignedUpdate(byteArray).pure[IO]
      }
      DecodeResult.success(signed)
    }
  }
}