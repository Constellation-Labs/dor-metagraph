package com.my.dor_metagraph.l0

import cats.effect.{IO, Resource}
import cats.syntax.option._
import com.my.dor_metagraph.l0.rewards.DorRewards
import com.my.dor_metagraph.l0.rewards.validators.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.l0.rewards.bounties.{DailyBountyRewards, AnalyticsBountyRewards}
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, DeviceInfo, DorAPIResponse, AnalyticsBountyInformation}
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.security.SecurityProvider
import weaver.MutableIOSuite
import eu.timepit.refined.auto._

object BuildRewardsTest extends MutableIOSuite {
  override type Res = SecurityProvider[IO]

  override def sharedResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

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

    test("Build correctly rewards - UnitDeployedBounty") {
      val currentAddress = Address("DAG0DQPuvVThrCnz66S4V6cocrtpg59oesAWyRMb")
      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none)
      val currentEpochProgress = 1440L

      val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress, none)))
      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

      for {
        rewards <- DorRewards.buildRewards(calculatedState, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, DailyBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(7, rewards.size) &&
        expect.eql(5400000000L, reward.get.amount.value.value) &&
        expect.eql(currentAddress, reward.get.destination)
    }

    test("Build correctly rewards - CommercialLocationBounty") {
      val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none)
      val currentEpochProgress = 1441L

      val state = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none)))
      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

      for {
        rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, DailyBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(7, rewards.size) &&
        expect.eql(5400000000L, reward.get.amount.value.value) &&
        expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    }

    test("Build correctly rewards - AnalyticsSubscriptionBounty") {
      val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, "123".some, "1".some, 10L.some)
      val currentEpochProgress = 3000L

      val state = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(3000L, "1", "123", 10L).some)))
      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(1))))

      for {
        rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, AnalyticsBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(7, rewards.size) &&
        expect.eql(22500000000L, reward.get.amount.value.value) &&
        expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    }

  test("Build correctly rewards - AnalyticsSubscriptionBounty - get collateral average when more than 1 device") {
    val currentAddress = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
    val currentAddress2 = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oepAWyRMb")

    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, "123".some, "1".some, 10L.some)
    val currentDeviceInfoAPIResponse2 = DorAPIResponse(currentAddress2.some, isInstalled = true, "Retail".some, "123".some, "1".some, 10L.some)

    val currentEpochProgress = 3000L

    val state = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(3000L, "1", "123", 10L).some),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse2, currentEpochProgress, AnalyticsBountyInformation(3000L, "1", "123", 10L).some)
    ))

    val balances = Map(
      currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(300000))),
      currentAddress2 -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(150000)))
    )

    for {
      rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, AnalyticsBountyRewards.make)
      reward = rewards.find(reward => reward.destination == currentAddress)
    } yield expect.eql(7, rewards.size) &&
      expect.eql(27000000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress.value.value, "DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")
  }

    test("Build correctly rewards - reward address null") {
      val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
      val currentDeviceInfoAPIResponse = DorAPIResponse(None, isInstalled = true, "Retail".some, none, none, none)
      val currentEpochProgress = 1440L

      val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress, none)))
      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

      for {
        rewards <- DorRewards.buildRewards(calculatedState, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, DailyBountyRewards.make)
      } yield expect.eql(0, rewards.size)
    }

    test("Build correctly rewards - multiple wallets with same reward address") {
      val currentAddress = Address("DAG0DCPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
      val currentAddress2 = Address("DAG0DAPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none)
      val currentEpochProgress = 1440L

      val state = CheckInDataCalculatedState(Map(
        currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none),
        currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none)
      ))

      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(210000))))

      for {
        rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, DailyBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(7, rewards.size) &&
        expect.eql(9900000000L, reward.get.amount.value.value) &&
        expect.eql(currentAddress, reward.get.destination) &&
        expect.eql(183333333L, rewards.toList(1).amount.value.value)
    }

    test("Build correctly rewards - validator nodes rewards successfully") {
      val validatorNodesL0 = getValidatorNodesL0
      val validatorNodesL1 = getValidatorNodesL1

      for {
        rewards <- getValidatorNodesTransactions(validatorNodesL0, validatorNodesL1, 50000)
      } yield expect.eql(6, rewards.size) &&
        forEach(rewards)(reward => expect.eql(8333L, reward.amount.value.value))
    }

    test("Build correctly rewards - same address validatorReward and bountyReward") {
      val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
      val currentAddress2 = Address("DAG0DQCuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none)
      val currentEpochProgress = 1440L

      val state = CheckInDataCalculatedState(Map(
        currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none),
        currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none)
      ))

      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(210000))))

      for {
        rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, DailyBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(6, rewards.size) &&
        expect.eql(10083333333L, reward.get.amount.value.value) &&
        expect.eql(currentAddress, reward.get.destination) &&
        expect.eql(183333333L, rewards.toList(1).amount.value.value)
    }

    test("Build correctly rewards - pay only for one device of team AnalyticsSubscriptionBounty") {
      val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oefAWyRMb")
      val currentAddress2 = Address("DAG0DQCuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

      val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, "123".some, "1".some, 10L.some)
      val currentEpochProgress = 3000L

      val state = CheckInDataCalculatedState(Map(
        currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(3000L, "1", "123", 10L).some),
        currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(3000L, "1", "123", 10L).some)
      ))

      val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(1))))

      for {
        rewards <- DorRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1, AnalyticsBountyRewards.make)
        reward = rewards.find(reward => reward.destination == currentAddress)
      } yield expect.eql(7, rewards.size) &&
        expect.eql(22500000000L, reward.get.amount.value.value) &&
        expect.eql(currentAddress, reward.get.destination) &&
        expect.eql(416666666L, rewards.toList(1).amount.value.value)
    }
}