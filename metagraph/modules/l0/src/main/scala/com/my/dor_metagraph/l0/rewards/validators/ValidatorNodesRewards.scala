package com.my.dor_metagraph.l0.rewards.validators

import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.my.dor_metagraph.shared_data.Utils.{PosLongEffectOps, RewardTransactionOps}
import org.tessellation.schema.address.Address
import org.tessellation.schema.transaction.RewardTransaction
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger


object ValidatorNodesRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("ValidatorNodesRewards")

  private def getRewardsValidatorNodes[F[_] : Async](
    addresses     : List[Address],
    taxToEachLayer: Long,
    layer         : String
  ): F[List[RewardTransaction]] =
    for {
      numberOfAddresses <- addresses.size.pure[F]
      amountToEachAddress <- (taxToEachLayer / numberOfAddresses).toPosLong
      validatorNodesRewards = addresses.map(address => (address, amountToEachAddress).toRewardTransaction)
      _ <- logger.info(s"[Validator Nodes $layer] Total Rewards to be distributed: $taxToEachLayer")
      _ <- logger.info(s"[Validator Nodes $layer] Distributing $amountToEachAddress to each one of the $numberOfAddresses addresses")
    } yield validatorNodesRewards

  def getValidatorNodesTransactions[F[_] : Async](
    validatorNodesL0     : List[Address],
    validatorNodesL1     : List[Address],
    taxesToValidatorNodes: Long
  ): F[List[RewardTransaction]] = {
    if (taxesToValidatorNodes < 2) {
      List.empty[RewardTransaction].pure[F]
    } else {
      val taxToEachLayer = taxesToValidatorNodes / 2

      for {
        validatorNodesL0Rewards <- getRewardsValidatorNodes(validatorNodesL0, taxToEachLayer, "L0")
        validatorNodesL1Rewards <- getRewardsValidatorNodes(validatorNodesL1, taxToEachLayer, "L1")
      } yield validatorNodesL0Rewards ::: validatorNodesL1Rewards
    }
  }
}
