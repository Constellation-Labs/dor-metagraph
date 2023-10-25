package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
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

  case class CheckInProof(id: String, signature: String)
  object CheckInProof {
    implicit val encoder: Encoder[CheckInProof] = deriveEncoder
    implicit val decoder: Decoder[CheckInProof] = deriveDecoder
  }

  case class CheckInStateUpdate(deviceId: Address, dts: Long, proof: CheckInProof, checkInHash: String)

  object CheckInStateUpdate {
    implicit val encoder: Encoder[CheckInStateUpdate] = deriveEncoder
    implicit val decoder: Decoder[CheckInStateUpdate] = deriveDecoder
  }

  case class DeviceInfo(lastCheckIn: Long, dorAPIResponse: DorAPIResponse, nextEpochProgressToReward: Long)

  object DeviceInfo {
    implicit val encoder: Encoder[DeviceInfo] = deriveEncoder
    implicit val decoder: Decoder[DeviceInfo] = deriveDecoder
  }

  case class DeviceCheckInWithSignature(cbor: String, hash: String, id: String, sig: String)

  object DeviceCheckInWithSignature {
    implicit val encoder: Encoder[DeviceCheckInWithSignature] = deriveEncoder
    implicit val decoder: Decoder[DeviceCheckInWithSignature] = deriveDecoder
  }
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  object DeviceCheckInInfo {
    implicit val encoder: Encoder[DeviceCheckInInfo] = deriveEncoder
    implicit val decoder: Decoder[DeviceCheckInInfo] = deriveDecoder
  }
  case class CheckInStateOnChain(updates: List[CheckInStateUpdate]) extends DataOnChainState

  object CheckInStateOnChain {
    implicit val encoder: Encoder[CheckInStateOnChain] = deriveEncoder
    implicit val decoder: Decoder[CheckInStateOnChain] = deriveDecoder
  }
  case class CheckInDataCalculatedState(devices: Map[Address, DeviceInfo]) extends DataCalculatedState

  object CheckInDataCalculatedState {
    implicit val encoder: Encoder[CheckInDataCalculatedState] = deriveEncoder
    implicit val decoder: Decoder[CheckInDataCalculatedState] = deriveDecoder
  }
  case class CheckInUpdate(publicId: String, signature: String, dts: Long, dtmCheckInHash: String, maybeDorAPIResponse: Option[DorAPIResponse]) extends DataUpdate

  object CheckInUpdate {
    implicit val encoder: Encoder[CheckInUpdate] = deriveEncoder
    implicit val decoder: Decoder[CheckInUpdate] = deriveDecoder
  }
  case class ClusterInfoResponse(id: String, ip: String, publicPort: Long, p2pPort: Long, session: String, state: String)

  object ClusterInfoResponse {
    implicit val encoder: Encoder[ClusterInfoResponse] = deriveEncoder
    implicit val decoder: Decoder[ClusterInfoResponse] = deriveDecoder
  }
  case class DorAPIResponse(rewardAddress: Option[Address], isInstalled: Boolean, locationType: Option[String], billedAmountMonthly: Option[Long])

  object DorAPIResponse {
    implicit val encoder: Encoder[DorAPIResponse] = deriveEncoder
    implicit val decoder: Decoder[DorAPIResponse] = deriveDecoder
  }
}