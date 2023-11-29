package com.my.dor_metagraph.l0.custom_routes

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io._

object CustomRoutes {
  @derive(encoder, decoder)
  case class CalculatedStateResponse(ordinal: Long, calculatedState:CheckInDataCalculatedState)
  def getLatestCalculatedState(calculatedStateService: CalculatedStateService[IO]): IO[Response[IO]] = {
    val calculatedState = calculatedStateService.getCalculatedState
    val response = calculatedState.map(state => CalculatedStateResponse(state.ordinal.value.value, state.state))
    Ok(response.unsafeRunSync())
  }
}
