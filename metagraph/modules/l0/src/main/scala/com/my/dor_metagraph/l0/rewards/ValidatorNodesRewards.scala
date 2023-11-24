package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.implicits.catsSyntaxApplicativeId
import eu.timepit.refined.types.all.PosLong
import cats.implicits.{toFlatMapOps, toFunctorOps}
import org.tessellation.schema.address.Address
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable.ListBuffer

object ValidatorNodesRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("ValidatorNodesRewards")

  private def getRewardsValidatorNodes[F[_] : Async](
    addresses     : List[Address],
    taxToEachLayer: Long,
    layer         : String
  ): F[List[RewardTransaction]] = {
    val numberOfAddresses = addresses.size
    val amountToEachAddress = taxToEachLayer / numberOfAddresses
    val validatorNodesRewards = new ListBuffer[RewardTransaction]()
    for (address <- addresses) {
      validatorNodesRewards += RewardTransaction(
        address,
        TransactionAmount(PosLong.unsafeFrom(amountToEachAddress))
      )
    }

    for {
      _ <- logger.info(s"[Validator Nodes $layer] Total Rewards to be distributed: $taxToEachLayer")
      _ <- logger.info(s"[Validator Nodes $layer] Number of addresses: $numberOfAddresses")
      _ <- logger.info(s"[Validator Nodes $layer] Distributing $amountToEachAddress to each one of the $numberOfAddresses addresses")
    } yield validatorNodesRewards.toList
  }

  def getValidatorNodesTransactions[F[_] : Async](
    validatorNodesL0     : List[Address],
    validatorNodesL1     : List[Address],
    taxesToValidatorNodes: Long
  ): F[List[RewardTransaction]] = {
    if (taxesToValidatorNodes <= 0) {
      return List.empty[RewardTransaction].pure[F]
    }

    val taxToEachLayer = taxesToValidatorNodes / 2

    for {
      validatorNodesL0Rewards <- getRewardsValidatorNodes(validatorNodesL0, taxToEachLayer, "L0")
      validatorNodesL1Rewards <- getRewardsValidatorNodes(validatorNodesL1, taxToEachLayer, "L1")
    } yield validatorNodesL0Rewards ::: validatorNodesL1Rewards
  }
}
