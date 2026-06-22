package com.my.dor_metagraph.l0

import com.my.dor_metagraph.l0.rewards.collateral.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegLong
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Balance
import weaver.SimpleIOSuite

object CollateralTest extends SimpleIOSuite {
  pureTest("Calculate values with collateral: < 50K") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

    val balance = NonNegLong.from(toTokenAmountFormat(10)) match {
      case Left(_) => NonNegLong.MinValue
      case Right(value) => value
    }

    val balances = Map(currentAddress -> Balance(balance))
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(bountiesWithCollateral._2 == CollateralLessThan50KMultiplier) &&
      expect.eql(0L, bountiesWithCollateral._1(currentAddress).value.value)
  }

  pureTest("Calculate values with collateral: > 50K and < 100K") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")

    val balance = NonNegLong.from(toTokenAmountFormat(70000)) match {
      case Left(_) => NonNegLong.MinValue
      case Right(value) => value
    }

    val balances = Map(currentAddress -> Balance(balance))
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(bountiesWithCollateral._2 == CollateralBetween50KAnd100KMultiplier) &&
      expect.eql(0L, bountiesWithCollateral._1(currentAddress).value.value)
  }

  pureTest("Calculate values with collateral: > 100K and < 200K") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val balance = NonNegLong.from(toTokenAmountFormat(150000)) match {
      case Left(_) => NonNegLong.MinValue
      case Right(value) => value
    }

    val balances = Map(currentAddress -> Balance(balance))
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(bountiesWithCollateral._2 == CollateralBetween100KAnd200KMultiplier) &&
      expect.eql(0L, bountiesWithCollateral._1(currentAddress).value.value)
  }

  pureTest("Calculate values with collateral: > 200K") {
    val currentAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val balance = NonNegLong.from(toTokenAmountFormat(210000)) match {
      case Left(_) => NonNegLong.MinValue
      case Right(value) => value
    }

    val balances = Map(currentAddress -> Balance(balance))
    val bountiesWithCollateral = getDeviceCollateral(balances, currentAddress)

    expect(bountiesWithCollateral._2 == CollateralGreaterThan200KMultiplier) &&
      expect.eql(1000000000000L, bountiesWithCollateral._1(currentAddress).value.value)
  }

}