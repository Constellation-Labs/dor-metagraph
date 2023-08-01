package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object BountiesTest extends SimpleIOSuite {

  pureTest("Get correctly rewards UnitDeployedBounty") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DeviceInfoAPIResponse(address, linkedToStore = true, Some("Retail"))
    val bountyAmount = UnitDeployedBounty("UnitDeployedBounty").getBountyRewardAmount(deviceInfoAPIResponse)

    expect.eql(50, bountyAmount)
  }

  pureTest("Get correctly rewards CommercialLocationBounty - Retail store") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DeviceInfoAPIResponse(address, linkedToStore = true, Some("Retail"))
    val bountyAmount = CommercialLocationBounty("CommercialLocationBounty").getBountyRewardAmount(deviceInfoAPIResponse)

    expect.eql(50, bountyAmount)
  }

  pureTest("Get correctly rewards CommercialLocationBounty - Residential store") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DeviceInfoAPIResponse(address, linkedToStore = true, Some("Residential"))
    val bountyAmount = CommercialLocationBounty("CommercialLocationBounty").getBountyRewardAmount(deviceInfoAPIResponse)

    expect.eql(0, bountyAmount)
  }
}