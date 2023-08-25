package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.tessellation.currency.dataApplication.L0NodeContext
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import Combiners.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.Types.{DeviceCheckInWithSignature, CheckInState}
import com.my.dor_metagraph.shared_data.Utils.{buildSignedUpdate, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.Validations.deviceCheckInValidations
import fs2.Compiler.Target.forSync
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider

object Data {
  def validateData(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    updates.traverse { signedUpdate =>
      val checkInInfo = getDeviceCheckInInfo(signedUpdate.value.cbor)
      deviceCheckInValidations(checkInInfo, signedUpdate.proofs, oldState)
    }.map(_.reduce)
  }


  def combine(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[CheckInState] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    val epochProgressIO = context.getLastCurrencySnapshot.map(_.get.epochProgress)

    updates.foldLeftM(oldState) { (acc, signedUpdate) =>
      val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]
      for{
        epochProgress <- epochProgressIO
        address <- addressIO
      } yield combineDeviceCheckIn(acc, signedUpdate, epochProgress.value.value, address)
    }
  }

  def serializeState(state: CheckInState): IO[Array[Byte]] = IO {
    Utils.customStateSerialization(state)
  }

  def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInState]] = IO {
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