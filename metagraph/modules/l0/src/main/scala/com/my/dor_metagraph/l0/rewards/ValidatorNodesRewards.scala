package com.my.dor_metagraph.l0.rewards

import eu.timepit.refined.types.all.PosLong
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}

import scala.collection.mutable.ListBuffer

object ValidatorNodesRewards {
  private val validatorNodesRewards = ValidatorNodesRewards()
  def getValidatorNodesTransactions(validatorNodesL0: List[Address], validatorNodesL1: List[Address], taxesToValidatorNodes: Long): List[RewardTransaction] = {
    validatorNodesRewards.getValidatorNodesTransactions(validatorNodesL0, validatorNodesL1, taxesToValidatorNodes)
  }
}
case class ValidatorNodesRewards() {
  private val logger = LoggerFactory.getLogger(classOf[ValidatorNodesRewards])
  private def getRewardsValidatorNodes(addresses: List[Address], taxToEachLayer: Long, layer: String): List[RewardTransaction] = {
    val numberOfAddresses = addresses.size
    val amountToEachAddress = taxToEachLayer / numberOfAddresses
    val validatorNodesRewards = new ListBuffer[RewardTransaction]()
    for (address <- addresses) {
      validatorNodesRewards += RewardTransaction(
        address,
        TransactionAmount(PosLong.unsafeFrom(amountToEachAddress))
      )
    }
    logger.info(s"[Validator Nodes $layer] Total Rewards to be distributed: $taxToEachLayer")
    logger.info(s"[Validator Nodes $layer] Number of addresses: $numberOfAddresses")
    logger.info(s"[Validator Nodes $layer] Distributing $amountToEachAddress to each one of the $numberOfAddresses addresses")

    validatorNodesRewards.toList
  }

  def getValidatorNodesTransactions(validatorNodesL0: List[Address], validatorNodesL1: List[Address], taxesToValidatorNodes: Long): List[RewardTransaction] = {
    if (taxesToValidatorNodes <= 0) {
      return List.empty
    }

    val taxToEachLayer = taxesToValidatorNodes / 2

    logger.info(s"Rewards to distribute between validator nodes: $taxesToValidatorNodes")
    logger.info(s"Rewards to distribute between validator nodes L0: $taxToEachLayer")
    logger.info(s"Rewards to distribute between validator nodes L1: $taxToEachLayer")

    //Validator nodes L0
    val validatorNodesL0Rewards = getRewardsValidatorNodes(validatorNodesL0, taxToEachLayer, "L0")

    //Validator nodes L1
    val validatorNodesL1Rewards = getRewardsValidatorNodes(validatorNodesL1, taxToEachLayer, "L1")

    validatorNodesL0Rewards ::: validatorNodesL1Rewards
  }
}
