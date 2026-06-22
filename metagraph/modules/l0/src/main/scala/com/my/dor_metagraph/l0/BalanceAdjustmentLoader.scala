package com.my.dor_metagraph.l0

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.Decoder
import io.circe.parser.decode
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.security.hash.Hash

import scala.collection.immutable.SortedSet
import scala.io.Source
import scala.util.Try

object BalanceAdjustmentLoader {

  @derive(encoder, decoder, eqv, show)
  case class RawBalanceAdjustment(
    address: Address,
    reason: String,
    reference: List[Hash],
    increase: Option[Long] = None,
    deduct: Option[Long] = None
  )

  // Safe NonNegLong conversion: a negative/out-of-range value becomes a decode error instead of
  // throwing (the former NonNegLong.unsafeFrom would crash; Math.abs(Long.MinValue) is still
  // negative and would also crash). These values adjust balances that feed collateral/rewards.
  private def toAmount(field: String, value: Long): Either[String, Amount] =
    NonNegLong.from(value).left.map(_ => s"BalanceAdjustment $field must be non-negative, got $value").map(Amount(_))

  private def toDeductAmount(value: Long): Either[String, Amount] =
    if (value == Long.MinValue) Left("BalanceAdjustment deduct is out of range")
    else toAmount("deduct", Math.abs(value))

  implicit val balanceAdjustmentDecoder: Decoder[BalanceAdjustment] = {
    val rawDecoder = implicitly[Decoder[RawBalanceAdjustment]]

    rawDecoder.emap { raw =>
      val reasonResult: Either[String, BalanceAdjustmentReason] = raw.reason match {
        case "SpendTransactionNotApplied"            => Right(SpendTransactionNotApplied)
        case "SpendTransactionSourceNotApplied"      => Right(SpendTransactionSourceNotApplied)
        case "SpendTransactionDestinationNotApplied" => Right(SpendTransactionDestinationNotApplied)
        case other                                   => Left(s"Unknown BalanceAdjustmentReason: $other")
      }

      val increaseResult: Either[String, Option[Amount]] =
        raw.increase.fold[Either[String, Option[Amount]]](Right(None))(v => toAmount("increase", v).map(Some(_)))

      val deductResult: Either[String, Option[Amount]] =
        raw.deduct.fold[Either[String, Option[Amount]]](Right(None))(v => toDeductAmount(v).map(Some(_)))

      for {
        reason   <- reasonResult
        increase <- increaseResult
        deduct   <- deductResult
      } yield BalanceAdjustment(
        address = raw.address,
        reason = reason,
        reference = SortedSet(raw.reference: _*),
        increase = increase,
        deduct = deduct
      )
    }
  }

  def loadBalanceAdjustments(resourcePath: String): Try[List[BalanceAdjustment]] =
    Try {
      val jsonString = Source.fromResource(resourcePath).mkString
      decode[List[BalanceAdjustment]](jsonString) match {
        case Right(adjustments) => adjustments
        case Left(error)        => throw new RuntimeException(s"Failed to parse JSON: $error")
      }
    }
}
