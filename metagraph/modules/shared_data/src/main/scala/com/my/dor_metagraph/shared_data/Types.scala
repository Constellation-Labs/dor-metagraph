package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Bounties.Bounty
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.tessellation.currency.dataApplication.{DataState, DataUpdate}
import org.tessellation.schema.address.Address

object Types {
  val EPOCH_PROGRESS_1_DAY: Long = 60 * 24
  @derive(decoder, encoder)
  case class FootTraffic(timestamp: Long, direction: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInFormatted(ac: List[Long], dts: Long, footTraffics: List[FootTraffic])

  @derive(decoder, encoder)
  case class CheckInProof(id: String, signature: String)

  @derive(decoder, encoder)
  case class CheckInUpdates(deviceId: Address, lastCheckIn: DeviceCheckInFormatted, proof: CheckInProof)

  @derive(decoder, encoder)
  case class DeviceInfo(bounties: List[Bounty], lastCheckIn: Long, deviceApiResponse: DeviceInfoAPIResponse, nextEpochProgressToReward: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInWithSignature(cbor: String, id: String, sig: String) extends DataUpdate

  @derive(decoder, encoder)
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class CheckInState(updates: List[CheckInUpdates], devices: Map[Address, DeviceInfo]) extends DataState

  @derive(decoder, encoder)
  case class DeviceInfoAPIResponse(rewardAddress: Address, isInstalled: Boolean, locationType: Option[String], billedAmountMonthly: Option[Long])
}
