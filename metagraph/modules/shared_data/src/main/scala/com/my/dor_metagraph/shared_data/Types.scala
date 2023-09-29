package com.my.dor_metagraph.shared_data

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.tessellation.currency.dataApplication.{DataState, DataUpdate}
import org.tessellation.schema.address.Address

object Types {
  val EPOCH_PROGRESS_1_DAY: Long = 60 * 24

  //Friday, 1 September 2023 00:00:00
  val MINIMUM_CHECK_IN_TIMESTAMP = 1693526400L

  @derive(decoder, encoder)
  case class CheckInProof(id: String, signature: String)

  @derive(decoder, encoder)
  case class CheckInUpdates(deviceId: Address, dts: Long, proof: CheckInProof, checkInHash: String)

  @derive(decoder, encoder)
  case class DeviceInfo(lastCheckIn: Long, deviceApiResponse: DeviceInfoAPIResponse, nextEpochProgressToReward: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInWithSignature(cbor: String, id: String, sig: String) extends DataUpdate

  @derive(decoder, encoder)
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class CheckInState(updates: List[CheckInUpdates], devices: Map[Address, DeviceInfo]) extends DataState

  @derive(decoder, encoder)
  case class DeviceInfoAPIResponseWithHash(rewardAddress: Address, isInstalled: Boolean, locationType: Option[String], billedAmountMonthly: Option[Long], checkInHash: String)

  @derive(decoder, encoder)
  case class DeviceInfoAPIResponse(rewardAddress: Address, isInstalled: Boolean, locationType: Option[String], billedAmountMonthly: Option[Long])
}
