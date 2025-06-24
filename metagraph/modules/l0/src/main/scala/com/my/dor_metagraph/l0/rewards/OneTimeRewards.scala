package com.my.dor_metagraph.l0.rewards

import com.my.dor_metagraph.l0.rewards.CsvReader.readCsvFromResources
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.transaction.{RewardTransaction, TransactionAmount}
import io.constellationnetwork.syntax.sortedCollection.sortedSetSyntax

import scala.collection.immutable.SortedSet

object OneTimeRewards {
  private def toFixedPoint(decimal: BigDecimal): Long = (decimal * 1e8).toLong

  def buildOneTimeRewards(currentOrdinal: SnapshotOrdinal): SortedSet[RewardTransaction] = {
    currentOrdinal match {
      case o if o == SnapshotOrdinal.unsafeApply(15269700) =>
        val rewardAddresses = readCsvFromResources("missing_rewards_tess_v3_migration.csv")
        rewardAddresses.map{ case (addressAsString, amountAsBigDecimal) =>
          val address = refineV[DAGAddressRefined](addressAsString).toOption.map(Address(_))
          val amount = PosLong.from(toFixedPoint(amountAsBigDecimal)).toOption
          RewardTransaction(address.get, TransactionAmount(amount.get))
        }.toSortedSet
      case _ =>
        SortedSet.empty
    }
  }
}
