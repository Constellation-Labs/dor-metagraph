package com.my.dor_metagraph.l0.rewards

import cats.effect.{Async, IO}
import cats.syntax.all._
import com.my.dor_metagraph.l0.rewards.BountyRewards.getBountyRewardsTransactions
import com.my.dor_metagraph.l0.rewards.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.combiners.ValidatorNodes.getValidatorNodes
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, EPOCH_PROGRESS_1_DAY}
import org.slf4j.LoggerFactory
import org.tessellation.currency.dataApplication.DataCalculatedState
import org.tessellation.currency.l0.snapshot.CurrencySnapshotEvent
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction
import org.tessellation.schema.transaction.{RewardTransaction, Transaction}
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.sdk.infrastructure.consensus.trigger
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

import scala.collection.immutable.{SortedMap, SortedSet}

object MainRewards {
  private val logger = LoggerFactory.getLogger("MainRewards")

  def make[F[_] : Async](implicit sp: SecurityProvider[IO]): Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] =
    (lastArtifact: Signed[CurrencyIncrementalSnapshot], lastBalances: SortedMap[Address, Balance], _: SortedSet[Signed[Transaction]], consensusTrigger: ConsensusTrigger, _: Set[CurrencySnapshotEvent], maybeCalculatedState: Option[DataCalculatedState]) => {
      consensusTrigger match {
        case trigger.EventTrigger =>
          logger.info("Event trigger snapshot, skipping rewards")
          SortedSet.empty[transaction.RewardTransaction].pure
        case trigger.TimeTrigger =>
          logger.info("Time trigger snapshot, trying to reward")
          maybeCalculatedState match {
            case None =>
              logger.error("Could not get calculatedState, skipping rewards.")
              SortedSet.empty[transaction.RewardTransaction].pure
            case Some(calculatedState) =>
              val currentEpochProgress = lastArtifact.epochProgress.value.value + 1
              val epochProgressModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY

              if (epochProgressModulus > 2) {
                val emptyTransactions: SortedSet[RewardTransaction] = SortedSet.empty
                emptyTransactions.pure
              } else {
                logger.info(s"Calculated state got: $calculatedState")
                val (l0ValidatorNodes, l1ValidatorNodes) = getValidatorNodes
                logInitialRewardDistribution(epochProgressModulus)
                buildRewards(calculatedState.asInstanceOf[CheckInDataCalculatedState], currentEpochProgress, lastBalances, l0ValidatorNodes, l1ValidatorNodes)
              }
          }
      }
    }

  def buildRewards[F[_] : Async](state: CheckInDataCalculatedState, currentEpochProgress: Long, lastBalances: Map[Address, Balance], l0ValidatorNodes: List[Address], l1ValidatorNodes: List[Address]): F[SortedSet[RewardTransaction]] = {
    val bountyRewards = getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)

    val bountyTransactions = bountyRewards._1
    val taxesToValidatorNodes = bountyRewards._2

    val validatorNodesTransactions = getValidatorNodesTransactions(l0ValidatorNodes, l1ValidatorNodes, taxesToValidatorNodes)
    buildRewardsTransactionsSortedSet(bountyTransactions, validatorNodesTransactions).pure
  }

  private def buildRewardsTransactionsSortedSet(bountyTransactions: List[RewardTransaction], validatorNodesTransactions: List[RewardTransaction]): SortedSet[RewardTransaction] = {
    val allTransactions = bountyTransactions ::: validatorNodesTransactions
    val allTransactionsFiltered = allTransactions.filter(_.amount.value.value > 0)

    SortedSet(allTransactionsFiltered: _*)
  }

  private def logInitialRewardDistribution(epochProgressModulus: Long): Unit = {
    epochProgressModulus match {
      case 0L =>
        logger.info("Starting UnitDeployed bounty distribution")
      case 1L =>
        logger.info("Starting CommercialLocation bounty distribution")
      case 2L =>
        logger.info("Starting RetailAnalyticsSubscription bounty distribution")
      case _ =>
        logger.info("Invalid")
    }
  }
}