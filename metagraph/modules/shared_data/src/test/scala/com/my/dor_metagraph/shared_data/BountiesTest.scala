package com.my.dor_metagraph.shared_data

import cats.syntax.option._
import com.my.dor_metagraph.shared_data.bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object BountiesTest extends SimpleIOSuite {

  pureTest("Get correctly rewards UnitDeployedBounty") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val bountyAmount = new UnitDeployedBounty().getBountyRewardAmount(deviceInfoAPIResponse, 0L)

    expect.eql(50, bountyAmount)
  }

  pureTest("Get correctly rewards CommercialLocationBounty - Retail store") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val bountyAmount = new CommercialLocationBounty().getBountyRewardAmount(deviceInfoAPIResponse, 1L)

    expect.eql(50, bountyAmount)
  }

  pureTest("Get correctly rewards CommercialLocationBounty - Residential store") {
    val address = Address.fromBytes("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb".getBytes)
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Residential".some, none, none, none, none, none)
    val bountyAmount = new CommercialLocationBounty().getBountyRewardAmount(deviceInfoAPIResponse, 1L)

    expect.eql(0, bountyAmount)
  }
}