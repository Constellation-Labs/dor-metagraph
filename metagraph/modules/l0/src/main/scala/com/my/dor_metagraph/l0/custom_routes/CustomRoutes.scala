package com.my.dor_metagraph.l0.custom_routes

import cats.effect.Async
import cats.implicits.toFunctorOps
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedState.getCalculatedState
import com.my.dor_metagraph.shared_data.types.Types.CheckInDataCalculatedState
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.http4s.{HttpRoutes, Response}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.tessellation.http.routes.internal.{InternalUrlPrefix, PublicRoutes}
import eu.timepit.refined.auto._
import org.http4s.server.middleware.CORS

case class CustomRoutes[F[_] : Async]() extends Http4sDsl[F] with PublicRoutes[F] {
  @derive(encoder, decoder)
  case class CalculatedStateResponse(ordinal: Long, calculatedState:CheckInDataCalculatedState)
  def getLatestCalculatedState: F[Response[F]] = {
    val calculatedState = getCalculatedState
    val response = calculatedState.map(state => CalculatedStateResponse(state._1.value.value, state._2))
    Ok(response)
  }

  private val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "calculated-state" / "latest" => getLatestCalculatedState
  }

  val public: HttpRoutes[F] =
    CORS
      .policy
      .withAllowCredentials(false)
      .httpRoutes(routes)

  override protected def prefixPath: InternalUrlPrefix = "/"
}
