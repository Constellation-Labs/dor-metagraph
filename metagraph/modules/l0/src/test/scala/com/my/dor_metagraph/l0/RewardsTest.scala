package com.my.dor_metagraph.l0

import cats.effect.{IO, Resource}
import com.my.dor_metagraph.l0.BountyRewards.{calculateBountiesRewardsWithCollateral, getDeviceBountyRewardsAmount, getTaxesToValidatorNodes}
import com.my.dor_metagraph.l0.Rewards.buildRewards
import com.my.dor_metagraph.l0.ValidatorNodesRewards.getValidatorNodesTransactions
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.Types.{CheckInState, DeviceCheckInFormatted, DeviceInfo, FootTraffic}
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.security.SecurityProvider
import weaver.MutableIOSuite

object RewardsTest extends MutableIOSuite {

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
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1440L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val state = CheckInState(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))
    val facilitatorsAddresses = getValidatorNodesL0

    val rewardsIO = buildRewards(state, currentEpochProgress, balances, IO.pure(facilitatorsAddresses))

    for {
      rewards <- rewardsIO
    } yield expect.eql(4, rewards.size) &&
      expect.eql(5400000000L, rewards.head.amount.value.value) &&
      expect.eql(currentAddress.value.value, rewards.head.destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses.head.value.value, rewards.toList(1).destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses(1).value.value, rewards.toList(2).destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses(2).value.value, rewards.toList(3).destination.value.value)
  }

  test("Build correctly rewards - CommercialLocationBounty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1441L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val state = CheckInState(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))
    val facilitatorsAddresses = getValidatorNodesL0

    val rewardsIO = buildRewards(state, currentEpochProgress, balances, IO.pure(facilitatorsAddresses))

    for {
      rewards <- rewardsIO
    } yield expect.eql(4, rewards.size) &&
      expect.eql(5400000000L, rewards.head.amount.value.value) &&
      expect.eql(currentAddress.value.value, rewards.head.destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses.head.value.value, rewards.toList(1).destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses(1).value.value, rewards.toList(2).destination.value.value) &&
      expect.eql(100000000L, rewards.toList(1).amount.value.value) &&
      expect.eql(facilitatorsAddresses(2).value.value, rewards.toList(3).destination.value.value)
  }

  test("Empty rewards when epochProgress does not complete 1 day") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1500L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val state = CheckInState(Map(currentAddress -> DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)))
    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(toTokenAmountFormat(200000))))
    val facilitatorsAddresses = getValidatorNodesL0

    val rewardsIO = buildRewards(state, currentEpochProgress, balances, IO.pure(facilitatorsAddresses))

    for {
      rewards <- rewardsIO
    } yield expect.eql(0, rewards.size)
  }

  pureTest("Get bounty reward amount - UnitDeployedBounty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1440L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val deviceInfo = DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - CommercialLocationBounty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1441L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val deviceInfo = DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - Empty") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val currentBounties = List(UnitDeployedBounty("UnitDeployedBounty"), CommercialLocationBounty("CommercialLocationBounty"))
    val currentDeviceInfoAPIResponse = DeviceInfoAPIResponse(currentAddress, linkedToStore = true, Some("Retail"))
    val currentEpochProgress = 1450L
    val currentCheckInRaw = DeviceCheckInFormatted(List(1, 2, 3), 123456, List(FootTraffic(12345, 1), FootTraffic(12345, 1)))

    val deviceInfo = DeviceInfo(currentCheckInRaw, currentBounties, currentDeviceInfoAPIResponse, currentEpochProgress)

    val unitDeployedBountyAmount = getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(0L, unitDeployedBountyAmount)
  }

  pureTest("Calculate values with collateral: < 50K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(10)

    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(balance)))
    val bountiesWithCollateral = calculateBountiesRewardsWithCollateral(balances, currentAddress, balance)

    expect.eql(1000000000L, bountiesWithCollateral)
  }

  pureTest("Calculate values with collateral: > 50K and < 100K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(70000)

    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(balance)))
    val bountiesWithCollateral = calculateBountiesRewardsWithCollateral(balances, currentAddress, balance)

    expect.eql(7350000000000L, bountiesWithCollateral)
  }

  pureTest("Calculate values with collateral: > 100K and < 200K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(150000)

    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(balance)))
    val bountiesWithCollateral = calculateBountiesRewardsWithCollateral(balances, currentAddress, balance)

    expect.eql(16500000000000L, bountiesWithCollateral)
  }

  pureTest("Calculate values with collateral: > 200K") {
    val currentAddress = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val balance = toTokenAmountFormat(210000)

    val balances = Map(currentAddress -> Balance(NonNegLong.unsafeFrom(balance)))
    val bountiesWithCollateral = calculateBountiesRewardsWithCollateral(balances, currentAddress, balance)

    expect.eql(25200000000000L, bountiesWithCollateral)
  }

  pureTest("Successfully get taxed to validator nodes") {
    val taxes = getTaxesToValidatorNodes(10000)

    expect.eql(1000, taxes)
  }

  pureTest("Build validator nodes rewards successfully") {
    val validatorNodesL0 = getValidatorNodesL0
    val validatorNodesL1 = getValidatorNodesL1

    val rewards = getValidatorNodesTransactions(validatorNodesL0, validatorNodesL1, 50000)

    expect.eql(6, rewards.size) &&
      forEach(rewards)(reward => expect.eql(8333L, reward.amount.value.value))
  }
}