package com.my.dor_metagraph.shared_data.encoders

import com.my.dor_metagraph.shared_data.types.Types._
import io.circe.Encoder
import io.circe.generic.semiauto._

object Encoders {
  def dataEncoder: Encoder[CheckInUpdate] = deriveEncoder
  def calculatedStateEncoder: Encoder[CheckInDataCalculatedState] = deriveEncoder
}
