package com.my.dor_metagraph.shared_data.external_apis

import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import com.my.dor_metagraph.shared_data.types.Types.ClusterInfoResponse
import eu.timepit.refined.auto._
import io.circe.parser.decode
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object ClusterApi {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("ClusterApi")

  private def getValidatorNodesAddressesFromClusterInfo[F[_] : Async : SecurityProvider](
    url: String
  ): F[List[Address]] = {
    for {
      response <- Async[F].delay(requests.get(url))
      body = response.text()
      _ <- logger.info(s"API ($url) response $body")
      clusterInfo <- Async[F].fromEither(decode[List[ClusterInfoResponse]](body))
      decoded <- clusterInfo.traverse(nodeInfo => getDagAddressFromPublicKey(nodeInfo.id))
    } yield decoded
  }

  def getValidatorNodesAddresses[F[_] : Async : SecurityProvider](
    metagraphL0NodeUrl: String,
    dataL1NodeUrl     : String
  ): F[(List[Address], List[Address])] = {
    val defaultAddressesL0 = List(
      Address("DAG0o6WSyvc7XfzujwJB1e25mfyzgXoLYDD6wqnk"),
      Address("DAG4YD6rkExLwYyAZzwjYJMxe36PAptKuUKq9uc7"),
      Address("DAG4nBD5J3Pr2uHgtS1sa16PqemHrwCcvjdR31Xe")
    )

    val defaultAddressesL1 = List(
      Address("DAG5fqiGq9L5iLH5R5eV7gBjkucewrcaQ1jVnKYD"),
      Address("DAG6B5mBMoEu3Habtb2ts3QGUD2UquywrQSLSubU"),
      Address("DAG5uDuGhPuh4mQZGNLFCEcdy69txSF4iSfFbdWJ")
    )

    for {
      _ <- logger.info(s"Starting to fetch validator nodes of env $metagraphL0NodeUrl and $dataL1NodeUrl")
      l0ValidatorNodes <- getValidatorNodesAddressesFromClusterInfo(metagraphL0NodeUrl).handleErrorWith { err =>
        val message = s"Error when getting validator nodes for l0ValidatorNodes: ${err.getMessage}. Using the default addresses: $defaultAddressesL0"
        logger.warn(message).as(defaultAddressesL0)
      }
      l1ValidatorNodes <- getValidatorNodesAddressesFromClusterInfo(dataL1NodeUrl).handleErrorWith { err =>
        val message = s"Error when getting validator nodes for l1ValidatorNodes: ${err.getMessage}. Using the default addresses: $defaultAddressesL1"
        logger.warn(message).as(defaultAddressesL1)
      }
    } yield (l0ValidatorNodes, l1ValidatorNodes)
  }

}