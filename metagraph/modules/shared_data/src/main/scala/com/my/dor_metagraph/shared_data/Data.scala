package com.my.dor_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.tessellation.currency.dataApplication.{L0NodeContext, L1NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import Combiners.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInWithSignature}
import com.my.dor_metagraph.shared_data.Utils.{buildSignedUpdate, customStateDeserialization, customStateSerialization, customUpdateDeserialization, customUpdateSerialization, getByteArrayFromRequestBody, getDeviceCheckInInfo}
import com.my.dor_metagraph.shared_data.Validations.{deviceCheckInValidationsL0, deviceCheckInValidationsL1}
import fs2.Compiler.Target.forSync
import org.http4s.{DecodeResult, EntityDecoder, MediaType}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.ID.Id
import org.tessellation.security.SecurityProvider
import org.tessellation.security.hex.Hex

object Data {

  def validateUpdate(update: DeviceCheckInWithSignature)(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    val lastCurrencySnapshotRaw = context.getLastCurrencySnapshot

    val addressIO = Id(Hex(update.id)).toAddress[IO]
    val lastCurrencySnapshotStateRawIO = lastCurrencySnapshotRaw.map(_.get.data)

    val checkInInfo = getDeviceCheckInInfo(update.cbor)

    for {
      address <- addressIO
      lastCurrencySnapshotRaw <- lastCurrencySnapshotStateRawIO
    } yield deviceCheckInValidationsL1(checkInInfo, lastCurrencySnapshotRaw, address)
  }
  def validateData(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    updates.traverse { signedUpdate =>
      val checkInInfo = getDeviceCheckInInfo(signedUpdate.value.cbor)
      deviceCheckInValidationsL0(checkInInfo, signedUpdate.proofs, oldState)
    }.map(_.reduce)
  }


  def combine(oldState: CheckInState, updates: NonEmptyList[Signed[DeviceCheckInWithSignature]])(implicit context: L0NodeContext[IO]): IO[CheckInState] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    val epochProgressIO = context.getLastCurrencySnapshot.map(_.get.epochProgress)

    updates.foldLeftM(oldState) { (acc, signedUpdate) =>
      val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]
      for {
        epochProgress <- epochProgressIO
        address <- addressIO
      } yield combineDeviceCheckIn(acc, signedUpdate, epochProgress.value.value + 1, address)
    }
  }

  def serializeState(state: CheckInState): IO[Array[Byte]] = IO {
    customStateSerialization(state)
  }

  def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, CheckInState]] = IO {
    customStateDeserialization(bytes)
  }

  def serializeUpdate(update: DeviceCheckInWithSignature): IO[Array[Byte]] = IO {
    customUpdateSerialization(update)
  }

  def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DeviceCheckInWithSignature]] = IO {
    customUpdateDeserialization(bytes)
  }

  def dataEncoder: Encoder[DeviceCheckInWithSignature] = deriveEncoder

  def dataDecoder: Decoder[DeviceCheckInWithSignature] = deriveDecoder

  def signedDataEntityDecoder: EntityDecoder[IO, Signed[DeviceCheckInWithSignature]] = {
    EntityDecoder.decodeBy(MediaType.text.plain) { msg =>
      val rawText = msg.as[String]
      val signed = rawText.flatMap { text =>
        println(s"Received RAW request: $text")
        val bodyAsBytes = getByteArrayFromRequestBody(text)
        buildSignedUpdate(bodyAsBytes).pure[IO]
      }
      println(s"PARSED RAW request: $signed")
      DecodeResult.success(signed)
    }
  }
}