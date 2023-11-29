package com.my.dor_metagraph.shared_data.calculated_state

import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import eu.timepit.refined.types.all.NonNegLong
import org.tessellation.schema.SnapshotOrdinal

case class CalculatedState(ordinal: SnapshotOrdinal, state: CheckInDataCalculatedState)

object CalculatedState {
  def empty: CalculatedState =
    CalculatedState(SnapshotOrdinal(NonNegLong(0L)), CheckInDataCalculatedState(Map.empty))
}
