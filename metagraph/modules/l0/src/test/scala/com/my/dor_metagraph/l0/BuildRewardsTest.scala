package com.my.dor_metagraph.l0

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.option._
import com.my.dor_metagraph.l0.rewards.DorRewards
import com.my.dor_metagraph.l0.rewards.bounties.{AnalyticsBountyRewards, DailyBountyRewards}
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodes
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.types.Types.{AnalyticsBountyInformation, CheckInDataCalculatedState, DeviceInfo, DorAPIResponse}
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import org.tessellation.node.shared.infrastructure.consensus.trigger.TimeTrigger
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.epoch.EpochProgress
import org.tessellation.schema.height.{Height, SubHeight}
import org.tessellation.schema.transaction.RewardTransaction
import org.tessellation.schema.{ActiveTip, BlockReference, DeprecatedTip, ID, SnapshotOrdinal, SnapshotTips}
import org.tessellation.security.hash.{Hash, ProofsHash}
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import weaver.SimpleIOSuite

import scala.collection.immutable.{SortedMap, SortedSet}

object BuildRewardsTest extends SimpleIOSuite {

  def getValidatorNodesL0: List[Address] = {
    List(
      Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      Address("DAG0DQQuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      Address("DAG0DQSuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    )
  }

  def getValidatorNodesL1: List[Address] = {
    List(
      Address("DAG0DQTuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      Address("DAG0DQUuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      Address("DAG0DQVuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    )
  }

  def getValidatorNodesService: ValidatorNodes[IO] = {
    new ValidatorNodes[IO] {
      override def getValidatorNodes: IO[(List[Address], List[Address])] = (
        getValidatorNodesL1,
        getValidatorNodesL0
      ).pure
    }
  }

  private def getCurrencyIncrementalSnapshot(
    hash         : Hash,
    epochProgress: Long
  ): Signed[CurrencyIncrementalSnapshot] =
    Signed(
      CurrencyIncrementalSnapshot(
        SnapshotOrdinal(NonNegLong(56L)),
        Height(123L),
        SubHeight(1L),
        hash,
        SortedSet.empty,
        SortedSet.empty,
        SnapshotTips(
          SortedSet(
            DeprecatedTip(BlockReference(Height(122L), ProofsHash("aaaa")), SnapshotOrdinal(55L)),
            DeprecatedTip(BlockReference(Height(122L), ProofsHash("cccc")), SnapshotOrdinal(55L))
          ),
          SortedSet(ActiveTip(BlockReference(Height(122L), ProofsHash("bbbb")), 2L, SnapshotOrdinal(55L)))
        ),
        stateProof = CurrencySnapshotStateProof(Hash(""), Hash("")),
        epochProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgress))
      ),
      NonEmptySet.one(SignatureProof(ID.Id(Hex("")), Signature(Hex(""))))
    )

  private def getRewards(
    lastArtifactEpochProgress: Long,
    calculatedState          : CheckInDataCalculatedState,
    balances                 : SortedMap[Address, Balance]
  ): IO[SortedSet[RewardTransaction]] = {
    DorRewards.make[IO](
      new DailyBountyRewards[IO],
      new AnalyticsBountyRewards[IO],
      getValidatorNodesService
    ).distribute(
      getCurrencyIncrementalSnapshot(Hash.empty, lastArtifactEpochProgress),
      balances,
      SortedSet.empty,
      TimeTrigger,
      Set.empty,
      calculatedState.some
    )
  }

  test("Build correctly rewards - UnitDeployedBounty") {
    val currentAddress = Address("DAG0DQPuvVThrCnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val lastArtifactEpochProgress = 1439L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none)))
    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(5400000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress, reward.get.destination)
  }

  test("Build correctly rewards - CommercialLocationBounty") {
    val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val lastArtifactEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none)))
    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(5400000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
  }

  test("Build correctly rewards - AnalyticsSubscriptionBounty") {
    val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    val lastArtifactEpochProgress = 1441L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, AnalyticsBountyInformation(1442L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)))
    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(1))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(22500000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
  }

  test("Build correctly rewards - AnalyticsSubscriptionBounty - get collateral average when more than 1 device") {
    val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    val currentAddress2 = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oepAWyRMb")

    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    val currentDeviceInfoAPIResponse2 = DorAPIResponse(currentAddress2.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)

    val lastArtifactEpochProgress = 1441L

    val calculatedState = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, AnalyticsBountyInformation(1442L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse2, lastArtifactEpochProgress, AnalyticsBountyInformation(1442L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)
    ))

    val balances = SortedMap(
      currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(300000))),
      currentAddress2 -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(150000)))
    )

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(27000000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
  }

  test("Build correctly rewards - reward address null") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(None, isInstalled = true, "Retail".some, none, none, none, none, none)
    val lastArtifactEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none)))
    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
    } yield expect.eql(0, rewards.size)
  }

  test("Build correctly rewards - multiple wallets with same reward address") {
    val currentAddress = Address("DAG0DCPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentAddress2 = Address("DAG0DAPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val lastArtifactEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none)
    ))

    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(210000))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(9900000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress, reward.get.destination) &&
      expect.eql(183333333L, rewards.toList(1).amount.value.value)
  }


  test("Build correctly rewards - same address validatorReward and bountyReward") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentAddress2 = Address("DAG0DQCuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val lastArtifactEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, none)
    ))

    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(210000))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(6, rewards.size) &&
      expect.eql(10083333333L, reward.get.amount.value.value) &&
      expect.eql(currentAddress, reward.get.destination) &&
      expect.eql(183333333L, rewards.toList(1).amount.value.value)
  }

  test("Build correctly rewards - pay only for one device of team AnalyticsSubscriptionBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oefAWyRMb")
    val currentAddress2 = Address("DAG0DQCuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val  rewardAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")

    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, rewardAddress.some)
    val lastArtifactEpochProgress = 1441L

    val calculatedState = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, AnalyticsBountyInformation(1442L, "1", 123L, 10L, rewardAddress.some).some),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, lastArtifactEpochProgress, AnalyticsBountyInformation(1442L, "1", 123L, 10L, rewardAddress.some).some)
    ))

    val balances = SortedMap(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(1))))

    for {
      rewards <- getRewards(lastArtifactEpochProgress, calculatedState, balances)
      analyticsReward = rewards.find(reward => reward.destination == rewardAddress)
      validatorsRewards = rewards.filter(reward => reward.destination != rewardAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(22500000000L, analyticsReward.get.amount.value.value) &&
      expect.eql(rewardAddress, analyticsReward.get.destination) &&
      expect.eql(validatorsRewards.size, 6) &&
      expect.eql(416666666L, validatorsRewards.toList(1).amount.value.value)
  }
}