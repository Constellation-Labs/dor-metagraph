package com.my.dor_metagraph.l0

import cats.syntax.option._
import com.my.dor_metagraph.l0.rewards.bounties.{AnalyticsBountyRewards, DailyBountyRewards}
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.auto._
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object BountyRewardsTest extends SimpleIOSuite {

  pureTest("Get bounty reward amount - UnitDeployedBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val currentEpochProgress = 1440L

    val deviceInfo = DeviceInfo(1693526401L, currentDeviceInfoAPIResponse, currentEpochProgress, none)
    val unitDeployedBountyAmount = new DailyBountyRewards().getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - CommercialLocationBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val currentEpochProgress = 1441L

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress, none)

    val unitDeployedBountyAmount = new DailyBountyRewards().getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(5000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - AnalyticsSubscriptionBounty") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    val currentEpochProgress = 2880L

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(2880L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)

    val unitDeployedBountyAmount = new AnalyticsBountyRewards().getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(25000000000L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - AnalyticsSubscriptionBounty - 0L when not reach correct epochProgress") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, 123L.some, "1".some, 10L.some, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some)
    val currentEpochProgress = 2880L

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress, AnalyticsBountyInformation(3500L, "1", 123L, 10L, Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb").some).some)

    val unitDeployedBountyAmount = new AnalyticsBountyRewards().getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(0L, unitDeployedBountyAmount)
  }

  pureTest("Get bounty reward amount - 0L when not reach correct epochProgress using modulus") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val currentDeviceInfoAPIResponse = DorAPIResponse(currentAddress.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val currentEpochProgress = 1450L

    val deviceInfo = DeviceInfo(123456L, currentDeviceInfoAPIResponse, currentEpochProgress, none)

    val unitDeployedBountyAmount = new DailyBountyRewards().getDeviceBountyRewardsAmount(deviceInfo, currentEpochProgress)
    expect.eql(0L, unitDeployedBountyAmount)
  }
}