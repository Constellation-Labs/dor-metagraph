package com.my.dor_metagraph.l0

import cats.effect.IO
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodesRewards.getValidatorNodesTransactions
import eu.timepit.refined.auto._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.transaction.RewardTransaction
import weaver.SimpleIOSuite

/**
  * The validator tax pool must be distributed with no minting and no burning: the sum of all
  * validator reward transactions must equal exactly the collected tax. These tests pin that
  * conservation invariant and the edge cases that used to crash (empty layer -> div-by-zero,
  * tiny tax -> PosLong.from(0)).
  */
object RewardConservationTest extends SimpleIOSuite {

  private val l0 = List(
    Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
    Address("DAG0DQQuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
    Address("DAG0DQSuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
  )
  private val l1 = List(
    Address("DAG0DQTuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
    Address("DAG0DQUuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
  )

  private def total(txs: List[RewardTransaction]): Long =
    txs.foldLeft(0L)((acc, tx) => acc + tx.amount.value.value)

  List(1L, 2L, 3L, 7L, 101L, 1000003L, 999_999_999L).foreach { tax =>
    test(s"validator tax pool is fully conserved (sum == tax) for tax=$tax") {
      getValidatorNodesTransactions[IO](l0, l1, tax).map(txs => expect.eql(tax, total(txs)))
    }
  }

  test("empty L0 layer redirects its share to L1 and still conserves the pool") {
    getValidatorNodesTransactions[IO](List.empty, l1, 101L).map(txs => expect.eql(101L, total(txs)))
  }

  test("zero tax pays nothing") {
    getValidatorNodesTransactions[IO](l0, l1, 0L).map(txs => expect.eql(0, txs.size))
  }

  test("tiny tax does not crash and emits only strictly-positive amounts") {
    getValidatorNodesTransactions[IO](l0, l1, 2L).map { txs =>
      expect.eql(2L, total(txs)) && expect(txs.forall(_.amount.value.value > 0L))
    }
  }
}
