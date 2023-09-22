package com.my.dor_metagraph.l0

import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.l0.BountyRewards.getBountyRewardsTransactions
import com.my.dor_metagraph.l0.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.Types.{CheckInState, EPOCH_PROGRESS_1_DAY}
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, Transaction}
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import com.my.dor_metagraph.shared_data.Utils.customStateDeserialization
import org.slf4j.LoggerFactory
import org.tessellation.schema.transaction
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.sdk.infrastructure.consensus.trigger

import scala.collection.immutable.{SortedMap, SortedSet}

object DorMetagraphRewards {
  def make[F[_] : Async : SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot] = (lastArtifact: Signed[CurrencyIncrementalSnapshot], lastBalances: SortedMap[Address, Balance], _: SortedSet[Signed[Transaction]], consensusTrigger: ConsensusTrigger) => {
    consensusTrigger match {
      case trigger.EventTrigger =>
        println("THIS IS A EVENT TRIGGER SNAPSHOT, SKIPPING REWARDS")
        SortedSet.empty[transaction.RewardTransaction].pure
      case trigger.TimeTrigger =>
        val lastSnapshotOrdinal = lastArtifact.value.ordinal.value.value
        println("THIS IS A TIME TRIGGER SNAPSHOT, TRYING TO REWARD")
        println(s"LAST SNAPSHOT ORDINAL: ${lastSnapshotOrdinal}. PROBABLY CURRENT ORDINAL: ${lastSnapshotOrdinal + 1}")
        val facilitatorsToReward = lastArtifact.proofs.map(_.id).toList.traverse(_.toAddress)
        lastArtifact.data.map(data => customStateDeserialization(data)) match {
          case None => SortedSet.empty[transaction.RewardTransaction].pure
          case Some(state) =>
            state match {
              case Left(_) => SortedSet.empty[transaction.RewardTransaction].pure
              case Right(state) =>
                DorMetagraphRewards().buildRewards(state, lastArtifact.epochProgress.value.value + 1, lastBalances, facilitatorsToReward)
            }
        }
    }
  }
}

case class DorMetagraphRewards() {
  private val logger = LoggerFactory.getLogger(classOf[DorMetagraphRewards])

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

  def buildRewards[F[_] : Async](state: CheckInState, currentEpochProgress: Long, lastBalances: Map[Address, Balance], facilitatorsAddresses: F[List[Address]]): F[SortedSet[RewardTransaction]] = {
    val epochProgressModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY

    /**
     * We have 3 bounties and they should be distributed in different snapshots. That's the reason for the conditional below:
     * UnitDeployed:                epochProgressModulus = 0
     * CommercialLocation:          epochProgressModulus = 1
     * RetailAnalyticsSubscription: epochProgressModulus = 2
     * */
    if (epochProgressModulus > 2) {
      val emptyTransactions: SortedSet[RewardTransaction] = SortedSet.empty
      return emptyTransactions.pure
    }

    logInitialRewardDistribution(epochProgressModulus)
    val bountyRewards = getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)

    val bountyTransactions = bountyRewards._1.pure
    val taxesToValidatorNodes = bountyRewards._2

    val validatorNodesL0 = facilitatorsAddresses
    val validatorNodesL1 = facilitatorsAddresses

    val validatorNodesTransactions = for {
      validatorNodesL0Addresses <- validatorNodesL0
      validatorNodesL1Addresses <- validatorNodesL1
    } yield getValidatorNodesTransactions(validatorNodesL0Addresses, validatorNodesL1Addresses, taxesToValidatorNodes)

    for {
      bountyTxns <- bountyTransactions
      validatorNodesTxns <- validatorNodesTransactions
    } yield buildRewardsTransactionsSortedSet(bountyTxns, validatorNodesTxns)
  }
}