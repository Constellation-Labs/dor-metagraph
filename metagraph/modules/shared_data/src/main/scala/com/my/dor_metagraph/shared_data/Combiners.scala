package com.my.dor_metagraph.shared_data

import cats.effect.IO
import Data.{CheckInRef, DeviceCheckInFormatted, DeviceCheckInRaw, DeviceInfo, DeviceUpdate, FootTraffic, State}
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.Bounties.{CommercialLocationBounty, UnitDeployedBounty}
import com.my.dor_metagraph.shared_data.DorApi.{DeviceInfoAPIResponse, fetchDeviceInfo}
import com.my.dor_metagraph.shared_data.Utils.customUpdateSerialization
import io.bullet.borer.Cbor
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.currency.dataApplication.L0NodeContext
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.tessellation.security.hash.Hash

object Combiners {
  def combine(
               acc: State,
               address: Address,
               deviceCheckIn: DeviceCheckInRaw,
               publicKey: String,
               snapshotOrdinal: Long,
               checkInHash: String,
               epochProgress: Long,
               deviceInfo: DeviceInfoAPIResponse
             ): State = {
    val state = acc.devices.get(address)
    val footTraffics = deviceCheckIn.e.map { event => FootTraffic(event.head, event.last) }
    val checkInRef = CheckInRef(snapshotOrdinal, checkInHash)
    val checkInFormatted = DeviceCheckInFormatted(deviceCheckIn.ac, deviceCheckIn.dts, footTraffics, checkInRef)

    val newState = state match {
      case Some(current) =>
        DeviceInfo(checkInFormatted, publicKey, current.bounties, deviceInfo, epochProgress)
      case None =>
        val bounties = List(
          UnitDeployedBounty("UnitDeployed"),
          CommercialLocationBounty("CommercialLocation")
        )
        DeviceInfo(checkInFormatted, publicKey, bounties, deviceInfo, epochProgress)
    }

    acc.focus(_.devices).modify(_.updated(address, newState))
  }

  def getDeviceCheckInFromCBOR(data: String): DeviceCheckInRaw = {
    try {
      val cborData = Utils.toCBORHex(data)
      Cbor.decode(cborData).to[DeviceCheckInRaw].value
    } catch {
      case e: Exception =>
        println(s"Error parsing data on check in. Data: $data")
        throw e
    }
  }

  def getCheckInHash(update: DeviceUpdate): String = Hash.fromBytes(customUpdateSerialization(update)).toString

  def combineDeviceCheckIn(acc: State, signedUpdate: Signed[DeviceUpdate])(implicit context: L0NodeContext[IO]): IO[State] = {
    try {
      implicit val sp: SecurityProvider[IO] = context.securityProvider
      val deviceCheckIn = getDeviceCheckInFromCBOR(signedUpdate.value.data)
      val checkInHash = getCheckInHash(signedUpdate.value)
      val publicKey = signedUpdate.proofs.head.id.hex.value

      val ordinalIO = context.getLastCurrencySnapshot.map(_.get.ordinal)
      val epochProgress = 1440L // Should be replaced
      val addressIO = signedUpdate.proofs.map(_.id).head.toAddress[IO]
      fetchDeviceInfo(publicKey) match {
        case Some(deviceInfo) =>
          for {
            ordinal <- ordinalIO
            address <- addressIO
          } yield combine(acc, address, deviceCheckIn, publicKey, ordinal.value.value, checkInHash, epochProgress, deviceInfo)
        case None => acc.pure[IO]
      }
    } catch {
      case e: Exception =>
        println(e.getMessage)
        println("Ignoring update and keeping with the current state")
        acc.pure[IO]
    }
  }
}
