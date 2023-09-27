package com.my.dor_metagraph.data_l1

import cats.data.OptionT
import cats.effect.IO
import cats.implicits.catsSyntaxOption
import com.my.dor_metagraph.shared_data.Data.deserializeState
import com.my.dor_metagraph.shared_data.Types.CheckInState
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io._
import org.tessellation.currency.dataApplication.L1NodeContext
import org.tessellation.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}

object CustomRoutes {

  @derive(decoder, encoder)
  case class SnapshotResponse(incrementalSnapshot: CurrencyIncrementalSnapshot, info: CurrencySnapshotInfo, snapshotState: CheckInState)

  private def getState(context: L1NodeContext[IO]) = {
    OptionT(context.getLastCurrencySnapshot)
      .flatMap(_.data.toOptionT)
      .flatMapF(deserializeState(_).map(_.toOption))
      .value
  }

  def getLatestSnapshotDecoded(implicit context: L1NodeContext[IO]): IO[Response[IO]] = {
    getState(context).flatMap {
      case None => NotFound()
      case Some(value) =>
        OptionT(context.getLastCurrencySnapshotCombined).map { snapshot =>
          SnapshotResponse(snapshot._1, snapshot._2, value)
        }.value.flatMap {
          case Some(value) => Ok(value)
          case None => NotFound()
        }
    }
  }

  def getSnapshotByOrdinalDecoded(ordinal: String)(implicit context: L1NodeContext[IO]): IO[Response[IO]] = {
    getState(context).flatMap {
      case None => NotFound()
      case Some(_) => Ok(ordinal)
    }
  }
}