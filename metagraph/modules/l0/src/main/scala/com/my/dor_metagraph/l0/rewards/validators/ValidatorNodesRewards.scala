package com.my.dor_metagraph.l0.rewards.validators

import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.functor._
import com.my.dor_metagraph.shared_data.Utils.{PosLongOps, RewardTransactionOps}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger


object ValidatorNodesRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("ValidatorNodesRewards")

  /**
    * Split `tax` datolites across `addresses` deterministically and WITHOUT loss:
    *   - each address gets `tax / n`,
    *   - the `tax % n` leftover datolites are handed out one each to the first `remainder`
    *     addresses in canonical (address-string) order.
    *
    * This conserves the pool exactly (`sum(payouts) == tax`) and never feeds a zero/negative value
    * into PosLong (addresses whose share would be 0 are simply omitted), so it cannot throw.
    */
  private def splitEvenly(
    addresses: List[Address],
    tax      : Long
  ): List[RewardTransaction] = {
    val sorted = addresses.distinct.sortBy(_.value.value)
    val n = sorted.size
    if (n == 0 || tax <= 0L) {
      List.empty
    } else {
      val base = tax / n
      val remainder = (tax % n).toInt
      sorted.zipWithIndex.flatMap { case (address, idx) =>
        val amount = base + (if (idx < remainder) 1L else 0L)
        if (amount > 0L) List((address, amount.toPosLongUnsafe).toRewardTransaction)
        else List.empty
      }
    }
  }

  def getValidatorNodesTransactions[F[_] : Async](
    validatorNodesL0     : List[Address],
    validatorNodesL1     : List[Address],
    taxesToValidatorNodes: Long
  ): F[List[RewardTransaction]] = {
    // splitEvenly conserves any tax >= 1 (the remainder is distributed datolite-by-datolite), so the
    // only no-op case is a zero pool.
    if (taxesToValidatorNodes < 1) {
      List.empty[RewardTransaction].pure[F]
    } else {
      // Split the pool 50/50 between layers; if a layer has no recipients its share is redirected to
      // the other so the full collected tax is always distributed (no minting, no burning).
      val (taxL0, taxL1) = (validatorNodesL0.nonEmpty, validatorNodesL1.nonEmpty) match {
        case (true, true) =>
          val half = taxesToValidatorNodes / 2
          // odd-datolite leftover goes to L0, keeping the total exact
          (taxesToValidatorNodes - half, half)
        case (true, false) => (taxesToValidatorNodes, 0L)
        case (false, true) => (0L, taxesToValidatorNodes)
        case (false, false) => (0L, 0L)
      }

      val l0Rewards = splitEvenly(validatorNodesL0, taxL0)
      val l1Rewards = splitEvenly(validatorNodesL1, taxL1)

      val logLine =
        if (validatorNodesL0.isEmpty && validatorNodesL1.isEmpty)
          // Misconfiguration: a non-zero pool with no recipients on either layer would be silently
          // burned. Make it loud rather than quietly under-distribute.
          logger[F].warn(s"[Validator Nodes] No validator addresses on either layer; tax=$taxesToValidatorNodes datolites was NOT distributed")
        else
          logger[F].info(
            s"[Validator Nodes] Distributing tax=$taxesToValidatorNodes -> L0=$taxL0 across ${validatorNodesL0.size} addresses, " +
              s"L1=$taxL1 across ${validatorNodesL1.size} addresses"
          )

      logLine.as(l0Rewards ::: l1Rewards)
    }
  }
}
