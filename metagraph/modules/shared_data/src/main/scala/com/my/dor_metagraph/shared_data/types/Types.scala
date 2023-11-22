package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.tessellation.currency.dataApplication.{DataCalculatedState, DataOnChainState, DataUpdate}
import org.tessellation.schema.address.Address

object Types {
  val EPOCH_PROGRESS_1_DAY: Long = 60 * 24

  //Friday, 1 September 2023 00:00:00
  val MINIMUM_CHECK_IN_TIMESTAMP = 1693526400L

  val COLLATERAL_50K: Long = toTokenAmountFormat(50000)
  val COLLATERAL_100K: Long = toTokenAmountFormat(100000)
  val COLLATERAL_200K: Long = toTokenAmountFormat(200000)

  val COLLATERAL_LESS_THAN_50K_MULTIPLIER: Double = 1
  val COLLATERAL_BETWEEN_50K_AND_100K_MULTIPLIER: Double = 1.05
  val COLLATERAL_BETWEEN_100K_AND_200K_MULTIPLIER: Double = 1.1
  val COLLATERAL_GREATER_THAN_200K_MULTIPLIER: Double = 1.2

  @derive(encoder, decoder)
  case class CheckInProof(id: String, signature: String)

  @derive(encoder, decoder)
  case class CheckInStateUpdate(deviceId: Address, dts: Long, proof: CheckInProof, checkInHash: String)

  @derive(encoder, decoder)
  case class DeviceInfo(lastCheckIn: Long, dorAPIResponse: DorAPIResponse, nextEpochProgressToReward: Long)

  @derive(encoder, decoder)
  case class DeviceCheckInWithSignature(cbor: String, hash: String, id: String, sig: String)

  @derive(encoder, decoder)
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(encoder, decoder)
  case class CheckInStateOnChain(updates: List[CheckInStateUpdate]) extends DataOnChainState

  @derive(encoder, decoder)
  case class CheckInDataCalculatedState(devices: Map[Address, DeviceInfo]) extends DataCalculatedState

  @derive(encoder, decoder)
  case class CheckInUpdate(publicId: String, signature: String, dts: Long, dtmCheckInHash: String, maybeDorAPIResponse: Option[DorAPIResponse]) extends DataUpdate

  @derive(encoder, decoder)
  case class ClusterInfoResponse(id: String, ip: String, publicPort: Long, p2pPort: Long, session: String, state: String)

  @derive(encoder, decoder)
  case class DorAPIResponse(rewardAddress: Option[Address], isInstalled: Boolean, locationType: Option[String], billedAmountMonthly: Option[Long])
}