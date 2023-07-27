package com.my.dor_metagraph.l0

import cats.effect.{Async, IO}
import com.my.dor_metagraph.shared_data.Bounties.{Bounty, CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Data.{DeviceInfo, State}
import org.tessellation.currency.schema.currency.{CurrencyBlock, CurrencyIncrementalSnapshot, CurrencySnapshotStateProof, CurrencyTransaction}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}
import org.tessellation.sdk.domain.rewards.Rewards
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.sdk.infrastructure.consensus.trigger.ConsensusTrigger
import com.my.dor_metagraph.shared_data.Utils.customStateDeserialization
import eu.timepit.refined.types.numeric.PosLong

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.ListBuffer

object Rewards {

  private def logBountyInformation(bounty: Bounty, address: Address, totalRewards: Long) = {
    println(s"[${bounty.name}] Reward Address ${address.value.value}. TotalRewards: ${totalRewards}")
  }
  private def getCurrentEpochProgress: Long = {
    1440L
  }

  private def getDeviceTotalRewards(device: DeviceInfo, currentEpochProgress: Long): Long = {
    val dayCurrentEpochProgress: Long = 60 * 24
    val epochModulus = currentEpochProgress % dayCurrentEpochProgress
    var deviceTotalRewards = 0L

    for (bounty <- device.bounties) {
      bounty match {
        case bounty: UnitDeployedBounty =>
          if (epochModulus == 0L) {
            deviceTotalRewards += bounty.getBountyRewardAmount(device.deviceApiResponse)
            logBountyInformation(bounty, device.deviceApiResponse.rewardAddress, deviceTotalRewards)
          }
        case bounty: CommercialLocationBounty =>
          if (epochModulus == 1L) {
            deviceTotalRewards += bounty.getBountyRewardAmount(device.deviceApiResponse)
            logBountyInformation(bounty, device.deviceApiResponse.rewardAddress, deviceTotalRewards)
          }
      }
    }

    deviceTotalRewards
  }

  private def buildRewards(state: State): SortedSet[RewardTransaction] = {
    val rewards = new ListBuffer[RewardTransaction]()
    val currentEpochProgress = getCurrentEpochProgress

    state.devices.map { case (_, value) =>
      val deviceTotalRewards = getDeviceTotalRewards(value, currentEpochProgress)
      if (deviceTotalRewards > 0) {
        rewards += RewardTransaction(
          value.deviceApiResponse.rewardAddress,
          TransactionAmount(PosLong.unsafeFrom(deviceTotalRewards * 10))
        )
      }
    }

    SortedSet(rewards.toList: _*)
  }

  def distributeRewards[F[_] : Async : SecurityProvider]: Rewards[cats.effect.IO, CurrencyTransaction, CurrencyBlock, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot] = (lastArtifact: Signed[CurrencyIncrementalSnapshot], lastBalances: SortedMap[Address, Balance], acceptedTransactions: SortedSet[Signed[CurrencyTransaction]], trigger: ConsensusTrigger) => {
    lastArtifact.data.map(data => customStateDeserialization(data)) match {
      case Some(state) =>
        state match {
          case Left(_) => IO.pure(SortedSet.empty)
          case Right(state) => IO.pure(buildRewards(state))
        }
      case None => IO.pure(SortedSet.empty)
    }
  }
}