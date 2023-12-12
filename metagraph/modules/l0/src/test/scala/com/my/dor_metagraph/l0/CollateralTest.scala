package com.my.dor_metagraph.l0

import com.my.dor_metagraph.l0.rewards.collateral.Collateral.getDeviceCollateral
import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegLong
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
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

    expect.eql(1.0, bountiesWithCollateral._2) &&
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

    expect.eql(1.05, bountiesWithCollateral._2) &&
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

    expect(1.1 == bountiesWithCollateral._2) &&
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

    expect(1.2 == bountiesWithCollateral._2) &&
      expect.eql(1000000000000L, bountiesWithCollateral._1(currentAddress).value.value)
  }

}