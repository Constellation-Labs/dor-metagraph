package com.my.dor_metagraph.l0

import cats.effect.{IO, Resource}
import com.my.dor_metagraph.l0.rewards.BountyRewards.{getDeviceBountyRewardsAmount, getDeviceCollateral, getTaxesToValidatorNodes}
import com.my.dor_metagraph.l0.rewards.MainRewards
import com.my.dor_metagraph.l0.rewards.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.types.Types.{CheckInDataCalculatedState, DeviceInfo, DorAPIResponse}
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.security.SecurityProvider
import weaver.MutableIOSuite
import eu.timepit.refined.auto._

object DorMetagraphRewardsTest extends MutableIOSuite {
  override type Res = SecurityProvider[IO]

  override def sharedResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  def getValidatorNodesL0: List[Address] = {
    List(
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM1".getBytes),
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM2".getBytes),
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM3".getBytes)
    )
  }

  def getValidatorNodesL1: List[Address] = {
    List(
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM4".getBytes),
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM5".getBytes),
      Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM6".getBytes)
    )
  }

  test("Build correctly rewards - UnitDeployedBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    val rewardsIO = MainRewards.buildRewards(calculatedState, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1)
    val rewardIO = rewardsIO.map(rewards => rewards.find(reward => reward.destination == currentAddress))

    for {
      rewards <- rewardsIO
      reward <- rewardIO
    } yield expect.eql(7, rewards.size) &&
      expect.eql(5400000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress, reward.get.destination)
  }

  test("Build correctly rewards - reward address null") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(None, isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L

    val calculatedState = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(123L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    val rewardsIO = MainRewards.buildRewards(calculatedState, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1)

    for {
      rewards <- rewardsIO
    } yield expect.eql(0, rewards.size)
  }

  test("Build correctly rewards - multiple wallets with same reward address") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentAddress2 = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRM2".getBytes)

    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L

    val state = CheckInDataCalculatedState(Map(
      currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress),
      currentAddress2 -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)
    ))

    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(210000))))

    val rewardsIO = MainRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1)
    val rewardIO = rewardsIO.map(rewards => rewards.find(reward => reward.destination == currentAddress))

    for {
      rewards <- rewardsIO
      reward <- rewardIO
    } yield expect.eql(7, rewards.size) &&
      expect.eql(9900000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress, reward.get.destination) &&
      expect.eql(9900000000L, rewards.toList(1).amount.value.value)
  }

  test("Build correctly rewards - CommercialLocationBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1441L

    val state = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    val rewardsIO = MainRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1)
    val rewardIO = rewardsIO.map(rewards => rewards.find(reward => reward.destination == currentAddress))

    for {
      rewards <- rewardsIO
      reward <- rewardIO
    } yield
      expect.eql(7, rewards.size) &&
      expect.eql(5400000000L, reward.get.amount.value.value) &&
      expect.eql(currentAddress.value.value, "DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
  }

  test("Empty rewards when epochProgress does not complete 1 day") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1500L

    val state = CheckInDataCalculatedState(Map(currentAddress -> DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))

    val rewardsIO = MainRewards.buildRewards(state, currentEpochProgress, balances, getValidatorNodesL0, getValidatorNodesL1)

    for {
      rewards <- rewardsIO
    } yield expect.eql(0, rewards.size)
  }

  pureTest("Get bounty reward amount - UnitDeployedBounty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1440L

    val deviceInfo = DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - CommercialLocationBounty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1441L

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - Empty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentDeviceInfoAPIResponse = DorAPIResponse(Some(currentAddress), isInstalled = true, Some("Retail"), Some(10L))
    val currentEpochProgress = 1450L
    //    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(0L, unitDeployedBountyAmount)
  }

  pureTest("Calculate values with collateral: < 50K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(10)

    val balances = Map(currentAddress -> balance)
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect.eql(1.0, bountiesWithCollateral._2) &&
    expect.eql(0L, bountiesWithCollateral._1(currentAddress))
  }

  pureTest("Calculate values with collateral: > 50K and < 100K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(70000)

    val balances = Map(currentAddress -> balance)
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect.eql(1.05, bountiesWithCollateral._2) &&
    expect.eql(0L, bountiesWithCollateral._1(currentAddress))
  }

  pureTest("Calculate values with collateral: > 100K and < 200K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(150000)

    val balances = Map(currentAddress -> balance)
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(1.1 == bountiesWithCollateral._2) &&
    expect.eql(0L, bountiesWithCollateral._1(currentAddress))
  }

  pureTest("Calculate values with collateral: > 200K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(210000)

    val balances = Map(currentAddress -> balance)
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(1.2 == bountiesWithCollateral._2) &&
      expect.eql(1000000000000L, bountiesWithCollateral._1(currentAddress))
  }

  pureTest("Build validator nodes rewards successfully") {
    val validatorNodesL0 = getValidatorNodesL0
    val validatorNodesL1 = getValidatorNodesL1

    val rewards = getValidatorNodesTransactions(validatorNodesL0, validatorNodesL1, 50000)

    expect.eql(6, rewards.size) &&
      forEach(rewards)(reward => expect.eql(8333L, reward.amount.value.value))
  }
}