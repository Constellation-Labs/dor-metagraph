package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.types.Types.{DeviceCheckInInfo, DeviceCheckInWithSignature}
import io.bullet.borer.derivation.MapBasedCodecs.{deriveCodec, deriveEncoder}
import io.bullet.borer.{Codec, Decoder, Encoder, Reader}
import org.tessellation.security.hex.Hex

object Codecs {
  private def readNextString(
    reader: Reader
  ): String = {
    reader.readString() //Discard first field (map key)
    Hex.fromBytes(reader.readByteArray()).value
  }

  implicit val decoderCheckInWithSignatureRaw: Decoder[DeviceCheckInWithSignature] =
    Decoder { reader =>
      val unbounded = reader.readMapOpen(4)
      val cbor = readNextString(reader)
      val hash = readNextString(reader)
      val id = readNextString(reader)
      val signature = readNextString(reader)

      val deviceCheckingWithSignature = DeviceCheckInWithSignature(cbor, hash, id, signature)
      reader.readArrayClose(unbounded, deviceCheckingWithSignature)
    }

  implicit val encoderCheckInWithSignatureRaw: Encoder[DeviceCheckInWithSignature] =
    deriveEncoder

  implicit val checkInfoCodec: Codec[DeviceCheckInInfo] =
    deriveCodec[DeviceCheckInInfo]
}
