package com.my.dor_metagraph.l0.rewards

import com.my.dor_metagraph.l0.rewards.CsvReader.readCsvFromResources
import com.my.dor_metagraph.shared_data.Utils.DatolitesPerDag
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.transaction.{RewardTransaction, TransactionAmount}
import io.constellationnetwork.syntax.sortedCollection.sortedSetSyntax

import scala.collection.immutable.SortedSet

object OneTimeRewards {
  // DAG (with decimals) -> datolites, exact integer conversion. Returns None (and the row is dropped)
  // if the value is fractional below datolite resolution or overflows Long, instead of throwing inside
  // the reward path and halting snapshot production.
  private def toFixedPoint(decimal: BigDecimal): Option[Long] =
    scala.util.Try((decimal * BigDecimal(DatolitesPerDag)).bigDecimal.longValueExact()).toOption

  def buildOneTimeRewards(currentOrdinal: SnapshotOrdinal): SortedSet[RewardTransaction] = {
    currentOrdinal match {
      case o if o == SnapshotOrdinal.unsafeApply(15269700) =>
        // Parse each row safely; a malformed address or non-positive/overflowing amount yields None and
        // is dropped deterministically (every node drops the same rows). The bundled resource is
        // validated by a test, so no rows are expected to drop.
        readCsvFromResources("missing_rewards_tess_v3_migration.csv").flatMap {
          case (addressAsString, amountAsBigDecimal) =>
            for {
              address  <- refineV[DAGAddressRefined](addressAsString).toOption.map(Address(_))
              rawAmount <- toFixedPoint(amountAsBigDecimal)
              amount   <- PosLong.from(rawAmount).toOption
            } yield RewardTransaction(address, TransactionAmount(amount))
        }.toSortedSet
      case _ =>
        SortedSet.empty
    }
  }
}
