package com.my.dor_metagraph.shared_data

import cats.ApplicativeError
import com.my.dor_metagraph.shared_data.types.Types._
import io.bullet.borer.Cbor
import org.tessellation.schema.ID.Id
import org.tessellation.security.hex.Hex

import scala.collection.mutable.ListBuffer
import org.tessellation.schema.address.Address
import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.bifunctor._
import cats.syntax.functor._
import com.my.dor_metagraph.shared_data.types.Codecs.checkInfoCodec
import eu.timepit.refined.types.all.PosLong
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger


object Utils {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Utils")

  def getByteArrayFromRequestBody(
    bodyAsString: String
  ): Array[Byte] = {
    val bodyAsBytes: ListBuffer[Byte] = ListBuffer.empty

    var idx = 0
    while (idx < bodyAsString.length) {
      val substringParsed = bodyAsString.substring(idx, idx + 2).trim
      val parsedString = s"0x$substringParsed"
      bodyAsBytes.addOne(Integer.decode(parsedString).toByte)
      idx = idx + 2
    }

    bodyAsBytes.toArray
  }

  def getDagAddressFromPublicKey[F[_] : Async : SecurityProvider](
    publicKeyHex: String
  ): F[Address] = {
    val publicKey: Id = Id(Hex(publicKeyHex))
    publicKey.toAddress[F]
  }

  private def toCBORHex[F[_] : Async](
    hexString: String
  ): F[Array[Byte]] = {
    if ((hexString.length & 1) != 0) {
      val message = "string length is not even"
      logger.error(message) >> new Exception(message).raiseError[F, Array[Byte]]
    } else {
      Async[F].delay(hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
    }
  }

  def toTokenAmountFormat(
    balance: Double
  ): Long = {
    (balance * 10e7).toLong
  }

  def getDeviceCheckInInfo[F[_] : Async](
    cborData: String
  ): F[DeviceCheckInInfo] = {
    for {
      checkInCborData <- toCBORHex(cborData).handleErrorWith { err =>
        val message = s"`$cborData` is not a valid hex string. Message: ${err.getMessage}"
        logger.error(message) >> new Exception(message).raiseError[F, Array[Byte]]
      }
      decodedCheckIn = Cbor.decode(checkInCborData).to[DeviceCheckInInfo].value
      _ <- logger.info(s"Decoded check-in AC: ${decodedCheckIn.ac}")
      _ <- logger.info(s"Decoded check-in DTS: ${decodedCheckIn.dts}")
      _ <- logger.info(s"Decoded check-in E: ${decodedCheckIn.e}")
    } yield decodedCheckIn
  }

  def getFirstAddressFromProofs[F[_] : Async : SecurityProvider](
    proofs: NonEmptySet[SignatureProof]
  ): F[Address] = {
    proofs.map(_.id).head.toAddress[F]
  }

  implicit class RewardTransactionOps(tuple: (Address, PosLong)) {
    def toRewardTransaction: RewardTransaction = {
      val (address, amount) = tuple
      RewardTransaction(address, TransactionAmount(amount))
    }
  }

  implicit class PosLongEffectOps[F[_]](value: Long)(implicit AE: ApplicativeError[F, Throwable]) {
    def toPosLong: F[PosLong] =
      AE.fromEither(PosLong.from(value).leftMap(m => new RuntimeException(m)))
  }

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)
  }
}