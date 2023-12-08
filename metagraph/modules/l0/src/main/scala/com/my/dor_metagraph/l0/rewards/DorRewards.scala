package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.syntax.all._
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.l0.rewards.bounties.{AnalyticsBountyRewards, BountyRewards, DailyBountyRewards}
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodes.getValidatorNodes
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.Utils.buildTransactionsSortedSet
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.currency.dataApplication.DataCalculatedState
import org.tessellation.currency.l0.snapshot.CurrencySnapshotEvent
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, Transaction}
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.sdk.infrastructure.consensus.trigger
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.{SortedMap, SortedSet}

object DorRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("MainRewards")

  def make[F[_] : Async : SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] = {
    (
      lastArtifact        : Signed[CurrencyIncrementalSnapshot],
      lastBalances        : SortedMap[Address, Balance],
      _                   : SortedSet[Signed[Transaction]],
      consensusTrigger    : ConsensusTrigger,
      _                   : Set[CurrencySnapshotEvent],
      maybeCalculatedState: Option[DataCalculatedState]
    ) => {
      consensusTrigger match {
        case trigger.EventTrigger => SortedSet.empty[RewardTransaction].pure[F]
        case trigger.TimeTrigger =>
          for {
            dailyRewards <- distributeDailyRewards(lastArtifact, lastBalances, maybeCalculatedState)
            analyticsRewards <- distributeAnalyticsRewards(lastArtifact, lastBalances, maybeCalculatedState)
          } yield buildTransactionsSortedSet(dailyRewards.toList, analyticsRewards.toList)
      }
    }
  }

  private def distributeDailyRewards[F[_] : Async : SecurityProvider](
    lastArtifact        : Signed[CurrencyIncrementalSnapshot],
    lastBalances        : SortedMap[Address, Balance],
    maybeCalculatedState: Option[DataCalculatedState]
  ): F[SortedSet[RewardTransaction]] = {
    val currentEpochProgress = lastArtifact.epochProgress.value.value + 1
    val epochProgressModulus = currentEpochProgress % EpochProgress1Day

    maybeCalculatedState
      .filter(_ => epochProgressModulus == ModulusInstallationBounty || epochProgressModulus == ModulusCommercialBounty)
      .map { calculatedState =>
        for {
          _ <- logger.info("Starting the daily rewards...")
          (l0ValidatorNodes, l1ValidatorNodes) <- getValidatorNodes
          checkInCalculatedState: CheckInDataCalculatedState = calculatedState.asInstanceOf[CheckInDataCalculatedState]
          rewards <- buildRewards(
            checkInCalculatedState,
            currentEpochProgress,
            lastBalances,
            l0ValidatorNodes,
            l1ValidatorNodes,
            DailyBountyRewards.make
          )
        } yield rewards
      }
      .getOrElse(SortedSet.empty[RewardTransaction].pure[F])
  }

  private def distributeAnalyticsRewards[F[_] : Async : SecurityProvider](
    lastArtifact        : Signed[CurrencyIncrementalSnapshot],
    lastBalances        : SortedMap[Address, Balance],
    maybeCalculatedState: Option[DataCalculatedState]
  ): F[SortedSet[RewardTransaction]] = {
    val currentEpochProgress = lastArtifact.epochProgress.value.value + 1
    val epochProgressModulus = currentEpochProgress % EpochProgress1Day

    maybeCalculatedState
      .filter(_ => epochProgressModulus == ModulusAnalyticsBounty)
      .map { calculatedState =>
        for {
          _ <- logger.info("Trying to distribute analytics rewards...")
          (l0ValidatorNodes, l1ValidatorNodes) <- getValidatorNodes
          checkInCalculatedState: CheckInDataCalculatedState = calculatedState.asInstanceOf[CheckInDataCalculatedState]
          rewards <- buildRewards(
            checkInCalculatedState,
            currentEpochProgress,
            lastBalances,
            l0ValidatorNodes,
            l1ValidatorNodes,
            AnalyticsBountyRewards.make
          )
        } yield rewards
      }
      .getOrElse(SortedSet.empty[RewardTransaction].pure[F])
  }

  def buildRewards[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalances        : Map[Address, Balance],
    l0ValidatorNodes    : List[Address],
    l1ValidatorNodes    : List[Address],
    bountyRewards       : BountyRewards[F]
  ): F[SortedSet[RewardTransaction]] = {
    for {
      bountyRewardsInfo <- bountyRewards.getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)
      bountyTransactions = bountyRewardsInfo.rewardTransactions
      taxesToValidatorNodes = bountyRewardsInfo.validatorsTaxes
      validatorNodesTransactions <- getValidatorNodesTransactions(l0ValidatorNodes, l1ValidatorNodes, taxesToValidatorNodes)
    } yield buildTransactionsSortedSet(bountyTransactions, validatorNodesTransactions)
  }
}