package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.l0.rewards.BountyRewards.getBountyRewardsTransactions
import com.my.dor_metagraph.l0.rewards.ValidatorNodesRewards.getValidatorNodesTransactions
import ValidatorNodes.getValidatorNodes
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.currency.dataApplication.DataCalculatedState
import org.tessellation.currency.l0.snapshot.CurrencySnapshotEvent
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, Transaction}
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.shared_data.Utils.{PosLongOps, RewardTransactionOps}
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.sdk.infrastructure.consensus.trigger
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.{MapView, View}
import scala.collection.immutable.{SortedMap, SortedSet}

object MainRewards {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("MainRewards")

  //TODO: Migrate the functions buildRewards, buildRewardsTransactionsSortedSet, logAllDevicesRewards, and logInitialRewardDistribution to inside make context. Tests need to be adapted
  def make[F[_] : Async : SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] = {
    (
      lastArtifact        : Signed[CurrencyIncrementalSnapshot],
      lastBalances        : SortedMap[Address, Balance],
      _                   : SortedSet[Signed[Transaction]],
      consensusTrigger    : ConsensusTrigger,
      _                   : Set[CurrencySnapshotEvent],
      maybeCalculatedState: Option[DataCalculatedState]
    ) => {
      def distributeRewards(
        lastArtifact        : Signed[CurrencyIncrementalSnapshot],
        lastBalances        : SortedMap[Address, Balance],
        maybeCalculatedState: Option[DataCalculatedState]
      ): F[SortedSet[RewardTransaction]] = {
        val currentEpochProgress = lastArtifact.epochProgress.value.value + 1
        val epochProgressModulus = currentEpochProgress % EpochProgress1Day

        maybeCalculatedState
          .filterNot( _ => epochProgressModulus < 0 || epochProgressModulus > 2)
          .map { calculatedState =>
            for {
              _ <- logger.info("Starting the rewards...")
              (l0ValidatorNodes, l1ValidatorNodes) <- getValidatorNodes
              _ <- logInitialRewardDistribution(epochProgressModulus)
              checkInCalculatedState: CheckInDataCalculatedState = calculatedState.asInstanceOf[CheckInDataCalculatedState]
              rewards <- buildRewards(checkInCalculatedState, currentEpochProgress, lastBalances, l0ValidatorNodes, l1ValidatorNodes)
            } yield rewards
          }
          .getOrElse(SortedSet.empty[RewardTransaction].pure[F])
      }

      consensusTrigger match {
        case trigger.EventTrigger => SortedSet.empty[RewardTransaction].pure[F]
        case trigger.TimeTrigger => distributeRewards(lastArtifact, lastBalances, maybeCalculatedState)
      }
    }
  }

  def buildRewards[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalances        : Map[Address, Balance],
    l0ValidatorNodes    : List[Address],
    l1ValidatorNodes    : List[Address]
  ): F[SortedSet[RewardTransaction]] = {
    for {
      bountyRewards <- getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)
      _ <- logAllDevicesRewards(bountyRewards)
      bountyTransactions = bountyRewards.rewardTransactions
      taxesToValidatorNodes = bountyRewards.validatorsTaxes
      validatorNodesTransactions <- getValidatorNodesTransactions(l0ValidatorNodes, l1ValidatorNodes, taxesToValidatorNodes)
    } yield buildRewardsTransactionsSortedSet(bountyTransactions, validatorNodesTransactions)
  }

  private def buildRewardsTransactionsSortedSet[F[_] : Async](
    bountyTransactions        : List[RewardTransaction],
    validatorNodesTransactions: List[RewardTransaction]
  ): SortedSet[RewardTransaction] = {
    val allTransactions = bountyTransactions ::: validatorNodesTransactions
    val groupedTransactions: MapView[Address, Long] =
      allTransactions
        .filter(_.amount.value.value > 0)
        .groupBy(_.destination)
        .view
        .mapValues(_.map(_.amount.value.value).sum)

    val summedTransactions: View[RewardTransaction] =
      groupedTransactions.map {
        case (address, totalAmount) =>
          (address, totalAmount.toPosLongUnsafe).toRewardTransaction
      }

    SortedSet.from(summedTransactions)
  }

  private def logAllDevicesRewards[F[_] : Async](
    bountyRewards: RewardTransactionsAndValidatorsTaxes
  ): F[Unit] = {
    def logRewardTransaction: RewardTransaction => F[Unit] = rewardTransaction =>
      logger.info(s"Device Reward Address: ${rewardTransaction.destination}. Amount: ${rewardTransaction.amount}")

    for {
      _ <- logger.info("All rewards to be distributed to devices")
      _ <- bountyRewards.rewardTransactions.traverse_(logRewardTransaction)
      _ <- logger.info(s"Validators taxes to be distributed between validators: ${bountyRewards.validatorsTaxes}")
    } yield ()
  }

  private def logInitialRewardDistribution[F[_] : Async](
    epochProgressModulus: Long
  ): F[Unit] = {
    epochProgressModulus match {
      case 0L => logger.info("Starting UnitDeployed bounty distribution")
      case 1L => logger.info("Starting CommercialLocation bounty distribution")
      case 2L => logger.info("Starting RetailAnalyticsSubscription bounty distribution")
      case _ => logger.info(s"Invalid epochProgressModulus $epochProgressModulus")
    }
  }
}