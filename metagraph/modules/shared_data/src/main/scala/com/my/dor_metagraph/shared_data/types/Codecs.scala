package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.Utils.convertBytesToHex
import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInInfo, DeviceCheckInWithSignature}
import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs.deriveCodec

object Codecs {
  implicit val decoderCheckInWithSignatureRaw: io.bullet.borer.Decoder[DeviceCheckInWithSignature] = io.bullet.borer.Decoder { reader =>
    val unbounded = reader.readMapOpen(4)
    reader.readString()
    val cbor = convertBytesToHex(reader.readByteArray())
    reader.readString()
    val hash = convertBytesToHex(reader.readByteArray())
    reader.readString()
    val id = convertBytesToHex(reader.readByteArray())
    reader.readString()
    val signature = convertBytesToHex(reader.readByteArray())

    val deviceCheckingWithSignature = DeviceCheckInWithSignature(cbor, hash, id, signature)
    reader.readArrayClose(unbounded, deviceCheckingWithSignature)
  }
  implicit val encoderCheckInWithSignatureRaw: io.bullet.borer.Encoder[DeviceCheckInWithSignature] = io.bullet.borer.derivation.MapBasedCodecs.deriveEncoder
  implicit val checkInfoCodec: Codec[DeviceCheckInInfo] = deriveCodec[DeviceCheckInInfo]
}
