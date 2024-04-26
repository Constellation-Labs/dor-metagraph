package com.my.dor_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.types.Types.CalculatedStateResponse
import eu.timepit.refined.auto._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.CORS
import org.http4s.{HttpRoutes, Response}
import org.tessellation.routes.internal.{InternalUrlPrefix, PublicRoutes}

case class CustomRoutes[F[_] : Async](calculatedStateService: CalculatedStateService[F]) extends Http4sDsl[F] with PublicRoutes[F] {

  private def getLatestCalculatedState: F[Response[F]] = {
    calculatedStateService.getCalculatedState
      .map(state => CalculatedStateResponse(state.ordinal.value.value, state.state))
      .flatMap(Ok(_))
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
