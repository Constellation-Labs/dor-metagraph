package com.my.dor_metagraph.l0.custom_routes

import cats.effect.Async
import cats.implicits.toFunctorOps
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.types.Types.CalculatedStateResponse
import org.http4s.{HttpRoutes, Response}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.tessellation.routes.internal.{InternalUrlPrefix, PublicRoutes}
import eu.timepit.refined.auto._
import org.http4s.server.middleware.CORS

case class CustomRoutes[F[_] : Async](calculatedStateService: CalculatedStateService[F]) extends Http4sDsl[F] with PublicRoutes[F] {

  private def getLatestCalculatedState: F[Response[F]] = {
    val calculatedState = calculatedStateService.getCalculatedState
    val response = calculatedState.map(state => CalculatedStateResponse(state.ordinal.value.value, state.state))
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
