package com.my.dor_metagraph.shared_data.external_apis

import cats.effect.Async
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import com.my.dor_metagraph.shared_data.types.Types.ClusterInfoResponse
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import eu.timepit.refined.auto._
import cats.implicits.{toFunctorOps, toFlatMapOps}

object ClusterApi {
  private val logger = LoggerFactory.getLogger("ClusterAPI")

  private def getValidatorNodesAddressesFromClusterInfo[F[_] : Async : SecurityProvider](url: String): F[List[Address]] = {
    try {
      logger.info(s"Fetching $url")
      val response = requests.get(url)
      val body = response.text()

      logger.info(s"API ($url) response $body")

      decode[List[ClusterInfoResponse]](body) match {
        case Left(err) =>
          logger.warn(s"Error when decoding ${err.getMessage}")
          List.empty[Address].pure[F]
        case Right(clusterInfo) =>
          clusterInfo.traverse(nodeInfo => getDagAddressFromPublicKey(nodeInfo.id))
      }
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching API: ${x.getMessage}")
        List.empty[Address].pure[F]
    }
  }

  def getValidatorNodesAddresses[F[_] : Async : SecurityProvider](metagraphL0NodeUrl: String, dataL1NodeUrl: String): F[(List[Address], List[Address])] = {
    logger.info(s"Starting to fetch validator nodes of env $metagraphL0NodeUrl and $dataL1NodeUrl")

    for {
      l0ValidatorNodes <- getValidatorNodesAddressesFromClusterInfo(metagraphL0NodeUrl)
      l1ValidatorNodes <- getValidatorNodesAddressesFromClusterInfo(dataL1NodeUrl)
      validatorNodesL0 = if (l0ValidatorNodes.isEmpty) {
        logger.info(s"Could not find l0 validator nodes on URL: $metagraphL0NodeUrl, using defaults")
        val addresses: List[Address] = List(Address("DAG0o6WSyvc7XfzujwJB1e25mfyzgXoLYDD6wqnk"), Address("DAG4YD6rkExLwYyAZzwjYJMxe36PAptKuUKq9uc7"), Address("DAG4nBD5J3Pr2uHgtS1sa16PqemHrwCcvjdR31Xe"))
        addresses
      } else {
        l0ValidatorNodes
      }
      validatorNodesL1 = if (l1ValidatorNodes.isEmpty) {
        logger.info(s"Could not find l1 validator nodes on URL: $dataL1NodeUrl, using defaults")
        val addresses: List[Address] = List(Address("DAG5fqiGq9L5iLH5R5eV7gBjkucewrcaQ1jVnKYD"), Address("DAG6B5mBMoEu3Habtb2ts3QGUD2UquywrQSLSubU"), Address("DAG5uDuGhPuh4mQZGNLFCEcdy69txSF4iSfFbdWJ"))
        addresses
      } else {
        l1ValidatorNodes
      }
    } yield (validatorNodesL0, validatorNodesL1)
  }
}