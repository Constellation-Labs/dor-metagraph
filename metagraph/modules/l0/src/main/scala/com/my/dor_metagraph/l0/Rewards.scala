package com.my.dor_metagraph.l0

import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Data.{DeviceInfo, State}
import org.tessellation.currency.schema.currency.{CurrencyBlock, CurrencyIncrementalSnapshot, CurrencySnapshotStateProof, CurrencyTransaction}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import com.my.dor_metagraph.shared_data.Utils.{customStateDeserialization, toTokenAmountFormat}
import eu.timepit.refined.types.numeric.PosLong
import org.tessellation.schema.transaction

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.ListBuffer

object Rewards {
  private val COLLATERAL_50K = toTokenAmountFormat(50000)
  private val COLLATERAL_100K = toTokenAmountFormat(100000)
  private val COLLATERAL_200K = toTokenAmountFormat(200000)

  private val COLLATERAL_LESS_THAN_50K_MULTIPLIER: Double = 1
  private val COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER: Double = 1.05
  private val COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER: Double = 1.1
  private val COLLATERAL_GREATER_THAN_200K_MULTIPLIER: Double = 1.2

  private val EPOCH_PROGRESS_1_DAY: Long = 60 * 24

  def getDeviceBountyRewardsAmount(device: DeviceInfo, currentEpochProgress: Long): Long = {
    val epochModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY
    var deviceTotalRewards = 0L

    for (bounty <- device.bounties) {
      bounty match {
        case bounty: UnitDeployedBounty =>
          if (epochModulus == 0L) {
            println(s"Bounty: ${bounty.name}")
            deviceTotalRewards += bounty.getBountyRewardAmount(device.deviceApiResponse)
          }
        case bounty: CommercialLocationBounty =>
          if (epochModulus == 1L) {
            println(s"Bounty: ${bounty.name}")
            deviceTotalRewards += bounty.getBountyRewardAmount(device.deviceApiResponse)
          }
      }
    }

    toTokenAmountFormat(deviceTotalRewards.toDouble)
  }

  def calculateBountiesRewardsWithCollateral(lastBalances: Map[Address, Balance], rewardAddress: Address, deviceTotalRewards: Long): Long = {
    val updatedBalance = lastBalances.get(rewardAddress) match {
      case Some(rewardAddressBalance) =>
        val balance = rewardAddressBalance.value.value
        if (balance < COLLATERAL_50K) {
          deviceTotalRewards * COLLATERAL_LESS_THAN_50K_MULTIPLIER
        } else if (balance >= COLLATERAL_50K && balance < COLLATERAL_100K) {
          deviceTotalRewards * COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER
        } else if (balance >= COLLATERAL_100K && balance < COLLATERAL_200K) {
          deviceTotalRewards * COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER
        } else {
          deviceTotalRewards * COLLATERAL_GREATER_THAN_200K_MULTIPLIER
        }
      case None => deviceTotalRewards.toDouble
    }

    updatedBalance.toLong
  }

  def getTaxesToValidatorNodes(deviceTotalRewards: Long): Long = {
    (deviceTotalRewards * 0.1).toLong
  }

  private def getDeviceBountiesRewards(device: DeviceInfo, currentEpochProgress: Long, lastBalances: Map[Address, Balance]): Long = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)

    calculateBountiesRewardsWithCollateral(lastBalances, device.deviceApiResponse.rewardAddress, deviceBountiesRewardsAmount)
  }

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
    println(s"[Validator Nodes $layer] Total Rewards to be distributed: $taxToEachLayer")
    println(s"[Validator Nodes $layer] Number of addresses: $numberOfAddresses")
    println(s"[Validator Nodes $layer] Distributing $amountToEachAddress to each one of the $numberOfAddresses addresses")

    validatorNodesRewards.toList
  }

  def buildValidatorNodesRewards(validatorNodesL0: List[Address], validatorNodesL1: List[Address], taxesToValidatorNodes: Long): List[RewardTransaction] = {
    val validatorNodesRewards = new ListBuffer[RewardTransaction]()
    val taxToEachLayer = taxesToValidatorNodes / 2

    println(s"Rewards to distribute between validator nodes: $taxesToValidatorNodes")
    println(s"Rewards to distribute between validator nodes L0: $taxToEachLayer")
    println(s"Rewards to distribute between validator nodes L1: $taxToEachLayer")

    //Validator nodes L0
    val validatorNodesL0Rewards = getRewardsValidatorNodes(validatorNodesL0, taxToEachLayer, "L0")
    validatorNodesRewards ++= validatorNodesL0Rewards

    //Validator nodes L1
    val validatorNodesL1Rewards = getRewardsValidatorNodes(validatorNodesL1, taxToEachLayer, "L1")
    validatorNodesRewards ++= validatorNodesL1Rewards

    validatorNodesRewards.toList
  }

  def buildRewards[F[_] : Async : SecurityProvider](state: State, currentEpochProgress: Long, lastBalances: Map[Address, Balance], facilitatorsAddresses: F[List[Address]]): F[SortedSet[RewardTransaction]] = {
    val allRewards = new ListBuffer[RewardTransaction]()
    var taxesToValidatorNodes = 0L

    state.devices.map { case (_, value) =>
      if (currentEpochProgress - value.lastCheckInEpochProgress > EPOCH_PROGRESS_1_DAY) {
        println(s"Device ${value.publicKey} didn't make a check in in the last 24 hours")
      } else {
        val deviceTotalRewards = getDeviceBountiesRewards(value, currentEpochProgress, lastBalances)
        val deviceTaxToValidatorNodes = getTaxesToValidatorNodes(deviceTotalRewards)
        val rewardValue = deviceTotalRewards - deviceTaxToValidatorNodes

        taxesToValidatorNodes += deviceTaxToValidatorNodes

        if (deviceTotalRewards > 0) {
          allRewards += RewardTransaction(
            value.deviceApiResponse.rewardAddress,
            TransactionAmount(PosLong.unsafeFrom(rewardValue))
          )
        }
      }
    }

    if (taxesToValidatorNodes > 0) {
      val validatorNodesL0 = facilitatorsAddresses
      val validatorNodesL1 = facilitatorsAddresses

      val rewardTransactions = for {
        validatorNodesL0Addresses <- validatorNodesL0
        validatorNodesL1Addresses <- validatorNodesL1
      } yield buildValidatorNodesRewards(validatorNodesL0Addresses, validatorNodesL1Addresses, taxesToValidatorNodes)

      rewardTransactions.map { rewardTransactions =>
        allRewards ++= rewardTransactions
        SortedSet(allRewards.toList: _*)
      }
    } else {
      SortedSet(allRewards.toList: _*).pure
    }
  }

  def distributeRewards[F[_] : Async : SecurityProvider]: Rewards[F, CurrencyTransaction, CurrencyBlock, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot] = (lastArtifact: Signed[CurrencyIncrementalSnapshot], lastBalances: SortedMap[Address, Balance], acceptedTransactions: SortedSet[Signed[CurrencyTransaction]], trigger: ConsensusTrigger) => {
    val facilitatorsToReward = lastArtifact.proofs.map(_.id).toList.traverse(_.toAddress)

    lastArtifact.data.map(data => customStateDeserialization(data)) match {
      case Some(state) =>
        state match {
          case Left(_) => SortedSet.empty[transaction.RewardTransaction].pure[F]
          case Right(state) => buildRewards(state, lastArtifact.epochProgress.value.value, lastBalances, facilitatorsToReward)
        }
      case None => SortedSet.empty[transaction.RewardTransaction].pure[F]
    }
  }
}