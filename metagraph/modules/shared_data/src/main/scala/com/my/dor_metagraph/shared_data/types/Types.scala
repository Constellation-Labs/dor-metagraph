package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.tessellation.currency.dataApplication.{DataCalculatedState, DataOnChainState, DataUpdate}
import org.tessellation.schema.address.Address
import org.tessellation.schema.balance.Balance
import org.tessellation.schema.transaction.RewardTransaction

object Types {
  val EpochProgress1Day: Long = 60 * 24

  val MinimumCheckInSeconds: Long =
    java.time.Instant.parse("2023-09-01T00:00:00.00Z").toEpochMilli / 1000L

  val Collateral50K: Long = toTokenAmountFormat(50 * 1000)
  val Collateral100K: Long = toTokenAmountFormat(100 * 1000)
  val Collateral200K: Long = toTokenAmountFormat(200 * 1000)

  val CollateralLessThan50KMultiplier: Double = 1D
  val CollateralBetween50KAnd100KMultiplier: Double = 1.05D
  val CollateralBetween100KAnd200KMultiplier: Double = 1.1D
  val CollateralGreaterThan200KMultiplier: Double = 1.2D

  @derive(encoder, decoder)
  case class CheckInProof(
    id       : String,
    signature: String
  )

  @derive(encoder, decoder)
  case class CheckInStateUpdate(
    deviceId   : Address,
    dts        : Long,
    proof      : CheckInProof,
    checkInHash: String
  )

  @derive(encoder, decoder)
  case class DeviceInfo(
    lastCheckIn              : Long,
    dorAPIResponse           : DorAPIResponse,
    nextEpochProgressToReward: Long
  )

  @derive(encoder, decoder)
  case class DeviceCheckInWithSignature(
    cbor: String,
    hash: String,
    id  : String,
    sig : String
  )

  @derive(encoder, decoder)
  case class DeviceCheckInInfo(
    ac : List[Long],
    dts: Long,
    e  : List[List[Long]]
  )

  @derive(encoder, decoder)
  case class CheckInStateOnChain(
    updates: List[CheckInStateUpdate]
  ) extends DataOnChainState

  @derive(encoder, decoder)
  case class CheckInDataCalculatedState(
    devices: Map[Address, DeviceInfo]
  ) extends DataCalculatedState

  @derive(encoder, decoder)
  case class CheckInUpdate(
    publicId           : String,
    signature          : String,
    dts                : Long,
    dtmCheckInHash     : String,
    maybeDorAPIResponse: Option[DorAPIResponse]
  ) extends DataUpdate

  @derive(encoder, decoder)
  case class ClusterInfoResponse(
    id        : String,
    ip        : String,
    publicPort: Long,
    p2pPort   : Long,
    session   : String,
    state     : String
  )

  @derive(encoder, decoder)
  case class DorAPIResponse(
    rewardAddress      : Option[Address],
    isInstalled        : Boolean,
    locationType       : Option[String],
    billedAmountMonthly: Option[Long]
  )

  @derive(encoder, decoder)
  case class CalculatedStateResponse(
    ordinal        : Long,
    calculatedState: CheckInDataCalculatedState
  )

  case class RewardTransactionsInformation(
    rewardTransactions: Map[Address, RewardTransaction],
    validatorsTaxes   : Long,
    lastBalances      : Map[Address, Balance]
  )

  case class RewardTransactionsAndValidatorsTaxes(
    rewardTransactions: List[RewardTransaction],
    validatorsTaxes   : Long
  )
}