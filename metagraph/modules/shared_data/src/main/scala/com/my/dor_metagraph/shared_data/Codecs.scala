package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.{DeviceCheckInInfo, DeviceCheckInWithSignature}
import com.my.dor_metagraph.shared_data.Utils.convertBytesToHex
import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs.deriveCodec

object Codecs {
  implicit val decoderCheckInWithSignatureRaw: io.bullet.borer.Decoder[DeviceCheckInWithSignature] = io.bullet.borer.Decoder { reader =>
    val unbounded = reader.readMapOpen(3)
    reader.readString()
    val cbor = convertBytesToHex(reader.readByteArray())
    reader.readString()
    val id = convertBytesToHex(reader.readByteArray())
    reader.readString()
    val signature = convertBytesToHex(reader.readByteArray())

    val deviceCheckingWithSignature = DeviceCheckInWithSignature(cbor, id, signature)
    reader.readArrayClose(unbounded, deviceCheckingWithSignature)
  }
  implicit val encoderCheckInWithSignatureRaw: io.bullet.borer.Encoder[DeviceCheckInWithSignature] = io.bullet.borer.derivation.MapBasedCodecs.deriveEncoder
  implicit val checkInfoCodec: Codec[DeviceCheckInInfo] = deriveCodec[DeviceCheckInInfo]
}
