package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxFlatMapOps, toFlatMapOps, toFunctorOps}
import com.my.dor_metagraph.l0.rewards.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.bounties.{CommercialLocationBounty, RetailAnalyticsSubscriptionBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.types.all.PosLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.{RewardTransaction, TransactionAmount}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.SelfAwareStructuredLogger

object BountyRewards {
  private val VALIDATOR_NODES_TAXES_PERCENT: Double = 0.1

  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("BountyRewards")

  def getDeviceBountyRewardsAmount(
    device              : DeviceInfo,
    currentEpochProgress: Long
  ): Long = {
    val epochModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY
    if (epochModulus == 0L) {
      toTokenAmountFormat(UnitDeployedBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
    } else if (epochModulus == 1L) {
      toTokenAmountFormat(CommercialLocationBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
    } else if (epochModulus == 2L) {
      toTokenAmountFormat(RetailAnalyticsSubscriptionBounty().getBountyRewardAmount(device.dorAPIResponse, epochModulus))
    } else {
      0L
    }
  }

  private def getDeviceBountiesRewards[F[_] : Async](
    device                    : DeviceInfo,
    currentEpochProgress      : Long,
    collateralMultiplierFactor: Double
  ): F[Long] = {
    val deviceBountiesRewardsAmount = getDeviceBountyRewardsAmount(device, currentEpochProgress)
    val rewardsWithCollateral = (deviceBountiesRewardsAmount * collateralMultiplierFactor).toLong
    logger.info(s"Device with reward address ${device.dorAPIResponse.rewardAddress}. Raw amount: $deviceBountiesRewardsAmount. Amount with collateral: ${rewardsWithCollateral}")
      .as(rewardsWithCollateral)
  }

  private def buildDeviceReward[F[_] : Async](
    rawRewardValue: Long,
    acc           : Map[Address, RewardTransaction],
    rewardAddress : Address
  ): F[Map[Address, RewardTransaction]] = {
    if (rawRewardValue <= 0) {
      logger.warn(s"Ignoring reward, value equals to 0").as(acc)
    } else {
      val rewardValue = acc
        .get(rewardAddress)
        .map { current =>
          val currentAmount: Long = current.amount.value.value
          currentAmount + rawRewardValue
        }
        .getOrElse(rawRewardValue)

      for {
        rewardValueFormatted <- PosLong.from(rewardValue) match {
          case Left(value) =>
            logger.warn(s"Error when parsing the value: $rewardValue. Response: $value").as(PosLong.MinValue)
          case Right(value) => value.pure[F]
        }
        rewardTransaction = RewardTransaction(rewardAddress, TransactionAmount(rewardValueFormatted))
        updatedTransactions = acc + (rewardAddress -> rewardTransaction)
      } yield updatedTransactions
    }
  }

  def getBountyRewardsTransactions[F[_] : Async](
    state               : CheckInDataCalculatedState,
    currentEpochProgress: Long,
    lastBalancesRaw     : Map[Address, Balance]
  ): F[RewardTransactionsAndValidatorsTaxes] = {
    val taxesToValidatorNodes: Long = 0L
    val allRewards: Map[Address, RewardTransaction] = Map.empty

    state.devices.foldLeft(RewardTransactionsInformation(allRewards, taxesToValidatorNodes, lastBalancesRaw).pure[F]) {
      case (accF, (_, deviceInfo)) =>
        deviceInfo.dorAPIResponse.rewardAddress match {
          case None =>
            logger.warn(s"Device doesn't have rewardAddress") >> accF
          case Some(rewardAddress) =>
            if (currentEpochProgress - deviceInfo.nextEpochProgressToReward > EPOCH_PROGRESS_1_DAY) {
              logger.warn(s"Device with reward address ${rewardAddress.value.value} didn't make a check in in the last 24 hours") >> accF
            } else {
              for {
                acc: RewardTransactionsInformation <- accF
                deviceCollateral: (Map[Address, Balance], Double) = getDeviceCollateral(acc.lastBalances, rewardAddress)

                updatedBalances: Map[Address, Balance] = deviceCollateral._1
                collateralMultiplierFactor: Double = deviceCollateral._2

                deviceTotalRewards: Long <- getDeviceBountiesRewards(deviceInfo, currentEpochProgress, collateralMultiplierFactor)
                deviceTaxToValidatorNodes: Long = (deviceTotalRewards * VALIDATOR_NODES_TAXES_PERCENT).toLong

                rewardValue: Long = deviceTotalRewards - deviceTaxToValidatorNodes

                deviceReward: Map[Address, RewardTransaction] <- buildDeviceReward(rewardValue, acc.rewardTransactions, rewardAddress)
                taxesToValidatorNodesUpdated: Long = acc.validatorsTaxes + deviceTaxToValidatorNodes

              } yield RewardTransactionsInformation(deviceReward, taxesToValidatorNodesUpdated, updatedBalances)
            }
        }
    }.map { rewardTransactionsWitValidatorsTaxes =>
      RewardTransactionsAndValidatorsTaxes(
        rewardTransactionsWitValidatorsTaxes.rewardTransactions.values.filter(_ != null).toList,
        rewardTransactionsWitValidatorsTaxes.validatorsTaxes
      )
    }
  }
}