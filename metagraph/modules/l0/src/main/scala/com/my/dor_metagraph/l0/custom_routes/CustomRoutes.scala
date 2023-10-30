package com.my.dor_metagraph.l0.custom_routes

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedState.getCalculatedState
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io._

object CustomRoutes {
  @derive(encoder, decoder)
  case class CalculatedStateResponse(ordinal: Long, calculatedState:CheckInDataCalculatedState)
  def getLatestCalculatedState: IO[Response[IO]] = {
    val calculatedState = getCalculatedState
    val response = calculatedState.map(state => CalculatedStateResponse(state._1.value.value, state._2))
    Ok(response.unsafeRunSync())
  }
}
