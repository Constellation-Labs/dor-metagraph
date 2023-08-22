package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Bounties.Bounty
import com.my.dor_metagraph.shared_data.DorApi.DeviceInfoAPIResponse
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.tessellation.currency.dataApplication.{DataState, DataUpdate}
import org.tessellation.schema.address.Address

object Types {
  @derive(decoder, encoder)
  case class FootTraffic(timestamp: Long, direction: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInFormatted(ac: List[Long], dts: Long, footTraffics: List[FootTraffic])

  @derive(decoder, encoder)
  case class DeviceInfo(lastCheckIn: DeviceCheckInFormatted, publicKey: String, bounties: List[Bounty], deviceApiResponse: DeviceInfoAPIResponse, lastCheckInEpochProgress: Long)

  @derive(decoder, encoder)
  case class DeviceCheckInWithSignature(cbor: String, id: String, sig: String) extends DataUpdate

  @derive(decoder, encoder)
  case class DeviceCheckInInfo(ac: List[Long], dts: Long, e: List[List[Long]])

  @derive(decoder, encoder)
  case class DeviceCheckInTransaction(owner: Address, snapshotOrdinal: Long)

  @derive(decoder, encoder)
  case class LastTxnRefs(snapshotOrdinal: Long, txnOrdinal: Long, hash: String)

  object LastTxnRefs {
    def empty: LastTxnRefs = LastTxnRefs(0, 0, "0000000000000000000000000000000000000000000000000000000000000000")
  }

  @derive(decoder, encoder)
  case class LastSnapshotRefs(ordinal: Long, hash: String)

  object LastSnapshotRefs {
    def empty: LastSnapshotRefs = LastSnapshotRefs(0, "0000000000000000000000000000000000000000000000000000000000000000")
  }

  @derive(decoder, encoder)
  case class CheckInState(devices: Map[Address, DeviceInfo], transactions: Map[String, DeviceCheckInTransaction], lastTxnRefs: Map[Address, LastTxnRefs], lastSnapshotRefs: Map[Address, LastSnapshotRefs]) extends DataState

}
