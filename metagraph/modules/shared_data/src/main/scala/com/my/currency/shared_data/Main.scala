package com.my.currency.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.currency.dataApplication.{DataState, DataUpdate, L1NodeContext}
import org.tessellation.security.signature.Signed
import cats.syntax.all._
import io.bullet.borer.{Cbor, Codec}
import monocle.syntax.all._
import org.tessellation.security.SecurityProvider
import io.bullet.borer.derivation.MapBasedCodecs._
import org.tessellation.schema.address.Address

object Data {
  implicit val dorCodec: Codec[Dor] = deriveCodec[Dor]

  @derive(decoder, encoder)
  case class Dor(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class DorUpdate(data: String) extends DataUpdate

  @derive(decoder, encoder)
  case class State(devices: Map[Address, Dor]) extends DataState

  def validateUpdate(update: DorUpdate)(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO {
    ().validNec
  }

  def validateData(oldState: State, updates: NonEmptyList[Signed[DorUpdate]])(implicit sp: SecurityProvider[IO]): IO[DataApplicationValidationErrorOr[Unit]] = IO {
    ().validNec
  }

  def combine(oldState: State, updates: NonEmptyList[Signed[DorUpdate]])(implicit sp: SecurityProvider[IO]): IO[State] = {
    updates.foldLeftM(oldState) { (acc, signedUpdate) =>
      val update = signedUpdate.value
      val cborData = Utils.toCBORHex(update.data)
      val newState: Dor = Cbor.decode(cborData).to[Dor].value

      signedUpdate.proofs.map(_.id).head.toAddress[IO].map{address =>
        acc.focus(_.devices).modify(_.updated(address, newState))
      }
    }
  }

  def serializeState(state: State): IO[Array[Byte]] = IO {
    Utils.customStateSerialization(state)
  }

  def deserializeState(bytes: Array[Byte]): IO[Either[Throwable, State]] = IO {
    Utils.customStateDeserialization(bytes)
  }

  def serializeUpdate(update: DorUpdate): IO[Array[Byte]] = IO {
    Utils.customUpdateSerialization(update)
  }

  def deserializeUpdate(bytes: Array[Byte]): IO[Either[Throwable, DorUpdate]] = IO {
    Utils.customUpdateDeserialization(bytes)
  }

  def dataEncoder: Encoder[DorUpdate] = deriveEncoder

  def dataDecoder: Decoder[DorUpdate] = deriveDecoder
}