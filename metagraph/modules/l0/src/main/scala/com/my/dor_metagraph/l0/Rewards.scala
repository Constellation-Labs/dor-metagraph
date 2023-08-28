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
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import com.my.dor_metagraph.shared_data.Utils.customStateDeserialization
import org.tessellation.schema.transaction

import scala.collection.immutable.{SortedMap, SortedSet}

object Rewards {

  private def buildRewardsTransactionsSortedSet(bountyTransactions: List[RewardTransaction], validatorNodesTransactions: List[RewardTransaction]): SortedSet[RewardTransaction] = {
    val allTransactions = bountyTransactions ::: validatorNodesTransactions
    val allTransactionsFiltered = allTransactions.filter(_.amount.value.value > 0)

    SortedSet(allTransactionsFiltered: _*)
  }

  def buildRewards[F[_] : Async](state: CheckInState, currentEpochProgress: Long, lastBalances: Map[Address, Balance], facilitatorsAddresses: F[List[Address]]): F[SortedSet[RewardTransaction]] = {
    if (currentEpochProgress % EPOCH_PROGRESS_1_DAY > 1) {
      val emptyTransactions: SortedSet[RewardTransaction] = SortedSet.empty
      return emptyTransactions.pure
    }

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

  def distributeRewards[F[_] : Async : SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot] = (lastArtifact: Signed[CurrencyIncrementalSnapshot], lastBalances: SortedMap[Address, Balance], _: SortedSet[Signed[Transaction]], _: ConsensusTrigger) => {
    val facilitatorsToReward = lastArtifact.proofs.map(_.id).toList.traverse(_.toAddress)

    lastArtifact.data.map(data => customStateDeserialization(data)) match {
      case None => SortedSet.empty[transaction.RewardTransaction].pure
      case Some(state) =>
        state match {
          case Left(_) => SortedSet.empty[transaction.RewardTransaction].pure
          case Right(state) => buildRewards(state, lastArtifact.epochProgress.value.value + 1, lastBalances, facilitatorsToReward)
        }
    }
  }
}