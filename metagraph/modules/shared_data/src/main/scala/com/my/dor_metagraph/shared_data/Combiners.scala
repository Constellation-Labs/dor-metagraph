package com.my.dor_metagraph.shared_data

import cats.effect.IO
import Data.{CheckInRef, DeviceCheckInFormatted, DeviceCheckInRaw, DeviceInfo, DeviceUpdate, FootTraffic, State}
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.fetchDeviceInfo
import com.my.dor_metagraph.shared_data.Utils.customUpdateSerialization
import io.bullet.borer.Cbor
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.currency.dataApplication.L0NodeContext
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.security.hash.Hash

object Combiners {
  private def combine(acc: State, address: Address, deviceCheckIn: DeviceCheckInRaw, publicKey: String, snapshotOrdinal: SnapshotOrdinal, checkInHash: String): State = {
    val state = acc.devices.get(address)
    fetchDeviceInfo(publicKey) match {
      case Some(deviceInfo) =>
        val footTraffics = deviceCheckIn.e.map { event => FootTraffic(event.head, event.last) }
        val checkInRef = CheckInRef(snapshotOrdinal.value.value, checkInHash)
        val checkInFormatted = DeviceCheckInFormatted(deviceCheckIn.ac, deviceCheckIn.dts, footTraffics, checkInRef)

        val newState = state match {
          case Some(current) =>
            DeviceInfo(checkInFormatted, publicKey, current.bounties, deviceInfo)
          case None =>
            val bounties = List(
              UnitDeployedBounty("UnitDeployed"),
              CommercialLocationBounty("CommercialLocation")
            )
            DeviceInfo(checkInFormatted, publicKey, bounties, deviceInfo)
        }

        acc.focus(_.devices).modify(_.updated(address, newState))
      case None => acc
    }
  }

  def combineDeviceCheckin(acc: State, signedUpdate: Signed[DeviceUpdate])(implicit context: L0NodeContext[IO]): IO[State] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider

    val cborData = Utils.toCBORHex(signedUpdate.value.data)
    val deviceCheckIn: DeviceCheckInRaw = Cbor.decode(cborData).to[DeviceCheckInRaw].value

    val checkInHash = Hash.fromBytes(customUpdateSerialization(signedUpdate.value)).toString
    val publicKey = signedUpdate.proofs.head.id.hex.value

    val ordinalIO = context.getLastCurrencySnapshot.map(_.get.ordinal)
    val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]

    for {
      ordinal <- ordinalIO
      address <- addressIO
    } yield
      combine(acc, address, deviceCheckIn, publicKey, ordinal, checkInHash)
  }
}
