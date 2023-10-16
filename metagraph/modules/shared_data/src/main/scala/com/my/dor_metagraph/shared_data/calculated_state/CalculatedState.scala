package com.my.dor_metagraph.shared_data.calculated_state

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState

object CalculatedState {
  private var maybeCheckInCalculatedState: CheckInDataCalculatedState = CheckInDataCalculatedState(Map.empty, List.empty, List.empty)
  def getCalculatedState: IO[CheckInDataCalculatedState] = {
    maybeCheckInCalculatedState.pure[IO]
  }

  def setCalculatedState(state: CheckInDataCalculatedState): IO[Boolean] = {
    maybeCheckInCalculatedState = state
    true.pure[IO]
  }
}
