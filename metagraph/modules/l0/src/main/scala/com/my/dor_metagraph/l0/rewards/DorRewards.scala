package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import cats.syntax.all._
import cats.syntax.functor.toFunctorOps
import com.my.dor_metagraph.l0.rewards.bounties.{AnalyticsBountyRewards, BountyRewards, DailyBountyRewards}
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodes
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.Utils.buildTransactionsSortedSet
import com.my.dor_metagraph.shared_data.types.Types._
import io.constellationnetwork.currency.dataApplication.DataCalculatedState
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import io.constellationnetwork.node.shared.domain.rewards.Rewards
import io.constellationnetwork.node.shared.infrastructure.consensus.trigger.{ConsensusTrigger, EventTrigger, TimeTrigger}
import io.constellationnetwork.node.shared.snapshot.currency.CurrencySnapshotEvent
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Balance
import io.constellationnetwork.schema.transaction.{RewardTransaction, Transaction}
import io.constellationnetwork.security.signature.Signed
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.{SortedMap, SortedSet}

object DorRewards {
  def make[F[_] : Async](
    dailyBountyRewards    : DailyBountyRewards[F],
    analyticsBountyRewards: AnalyticsBountyRewards[F],
    validatorNodes        : ValidatorNodes[F]
  ): Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] =
    (
      lastArtifact        : Signed[CurrencyIncrementalSnapshot],
      lastBalances        : SortedMap[Address, Balance],
      _                   : SortedSet[Signed[Transaction]],
      trigger             : ConsensusTrigger,
      _                   : Set[CurrencySnapshotEvent],
      maybeCalculatedState: Option[DataCalculatedState]
    ) => {
      val logger = Slf4jLogger.getLoggerFromName[F]("DorRewards")

      val currentEpochProgress: Long = lastArtifact.epochProgress.value.value + 1
      val epochProgressModulus = currentEpochProgress % EpochProgress1Day

      def noRewards: F[SortedSet[RewardTransaction]] = SortedSet.empty[RewardTransaction].pure[F]

      def toCheckInDataCalculatedState(state: DataCalculatedState): F[CheckInDataCalculatedState] =
        Async[F].delay(state.asInstanceOf[CheckInDataCalculatedState])

      def distributeDailyRewards(
        state: CheckInDataCalculatedState
      ): F[SortedSet[RewardTransaction]] =
        if (epochProgressModulus == ModulusInstallationBounty || epochProgressModulus == ModulusCommercialBounty)
          logger.info("Starting the daily rewards...") >> buildRewards(state, dailyBountyRewards)
        else
          noRewards

      def distributeAnalyticsRewards(
        state: CheckInDataCalculatedState
      ): F[SortedSet[RewardTransaction]] =
        if (epochProgressModulus == ModulusAnalyticsBounty)
          logger.info("Trying to distribute analytics rewards...") >> buildRewards(state, analyticsBountyRewards)
        else
          noRewards

      def buildRewards(
        state        : CheckInDataCalculatedState,
        bountyRewards: BountyRewards[F]
      ): F[SortedSet[RewardTransaction]] =
        for {
          (l0ValidatorNodes, l1ValidatorNodes) <- validatorNodes.getValidatorNodes
          rewardsInfo <- bountyRewards.getBountyRewardsTransactions(state, currentEpochProgress, lastBalances)
          validatorNodesTransactions <- getValidatorNodesTransactions(l0ValidatorNodes, l1ValidatorNodes, rewardsInfo.validatorsTaxes)
        } yield buildTransactionsSortedSet(rewardsInfo.rewardTransactions, validatorNodesTransactions)

      trigger match {
        case EventTrigger => noRewards
        case TimeTrigger =>
          maybeCalculatedState match {
            case None => noRewards
            case Some(dataCalculatedState) =>
              for {
                checkInDataCalculatedState <- toCheckInDataCalculatedState(dataCalculatedState)
                dailyRewards <- distributeDailyRewards(checkInDataCalculatedState)
                analyticsRewards <- distributeAnalyticsRewards(checkInDataCalculatedState)
              } yield buildTransactionsSortedSet(dailyRewards.toList, analyticsRewards.toList)
          }
      }
    }
}