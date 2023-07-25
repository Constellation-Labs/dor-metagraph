package com.my.dor_metagraph.shared_data

import cats.effect.IO
import MainData.{DeviceCheckin, DeviceCheckinWithEpochProgress, DeviceUpdate, State}
import io.bullet.borer.Cbor
import monocle.Monocle.toAppliedFocusOps
import org.tessellation.currency.dataApplication.L0NodeContext
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

object Combiners {
  def combineDeviceCheckin(acc: State, signedUpdate: Signed[DeviceUpdate])(implicit context: L0NodeContext[IO]): IO[State] = {
    implicit val sp: SecurityProvider[IO] = context.securityProvider
    val epochProgressIO = context.getLastGlobalSnapshot.map(_.get.epochProgress)

    val update = signedUpdate.value
    val cborData = Utils.toCBORHex(update.data)
    val deviceCheckin: DeviceCheckin = Cbor.decode(cborData).to[DeviceCheckin].value

    epochProgressIO.flatMap { epochProgress =>
      val newState = DeviceCheckinWithEpochProgress(deviceCheckin.ac, deviceCheckin.dts, deviceCheckin.e, epochProgress.value.value)
      signedUpdate.proofs.map(_.id).head.toAddress[IO].map { address =>
        acc.focus(_.devices).modify(_.updated(address, newState))
      }
    }
  }
}
