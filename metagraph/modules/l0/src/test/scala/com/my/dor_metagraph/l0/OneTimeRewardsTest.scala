package com.my.dor_metagraph.l0

import cats.syntax.all._
import com.my.dor_metagraph.l0.rewards.OneTimeRewards.buildOneTimeRewards
import com.my.dor_metagraph.shared_data.Utils.buildTransactionsSortedSet
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.transaction.{RewardTransaction, TransactionAmount}
import weaver.SimpleIOSuite

import scala.collection.immutable.SortedSet

object OneTimeRewardsTest extends SimpleIOSuite {
  pureTest("Should build correctly the one time rewards of tessellation v3 migration") {
    val oneTimeRewards = buildOneTimeRewards(SnapshotOrdinal.unsafeApply(15269700L))
    val parsedOneTimeRewards = buildTransactionsSortedSet(List.empty, oneTimeRewards.toList)
    expect.all(
      312 === parsedOneTimeRewards.size,
      parsedOneTimeRewards.head.destination === Address("DAG024nxd6ZRhULeYdUGtG1r8QD2e4nSr4VP86xc"),
      parsedOneTimeRewards.head.amount.value.value === 36360000000L
    )
  }

  pureTest("Should build correctly the one time rewards of tessellation v3 migration and concatenate with regular one") {
    val oneTimeRewards = buildOneTimeRewards(SnapshotOrdinal.unsafeApply(15269700L))
    val regularTransactions = SortedSet.from(
      List(
        RewardTransaction(Address("DAG024nxd6ZRhULeYdUGtG1r8QD2e4nSr4VP86xc"), TransactionAmount(PosLong.unsafeFrom(40000000)))
      )
    )
    val combinedRewards = buildTransactionsSortedSet(regularTransactions.toList, oneTimeRewards.toList)
    expect.all(
      312 === combinedRewards.size,
      combinedRewards.head.destination === Address("DAG024nxd6ZRhULeYdUGtG1r8QD2e4nSr4VP86xc"),
      combinedRewards.head.amount.value.value === 36400000000L
    )
  }

}