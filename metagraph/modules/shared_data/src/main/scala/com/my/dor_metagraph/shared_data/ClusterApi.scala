package com.my.dor_metagraph.shared_data

import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import com.my.dor_metagraph.shared_data.Types.ClusterInfoResponse
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

object ClusterApi {
  private val clusterApi = ClusterApi()

  def getValidatorNodesAddresses(metagraphL0NodeUrl: String, dataL1NodeUrl: String,  securityProvider: SecurityProvider[IO]): (IO[List[Address]], IO[List[Address]]) = {
    clusterApi.getValidatorNodesAddresses(metagraphL0NodeUrl, dataL1NodeUrl, securityProvider)
  }
}

case class ClusterApi() {
  private val logger = LoggerFactory.getLogger(classOf[ClusterApi])

  private def getValidatorNodesAddressesFromClusterInfo(url: String, sp: SecurityProvider[IO]): IO[List[Address]] = {
    try {
      logger.info(s"Fetching $url")
      val response = requests.get(url)
      val body = response.text()

      logger.info(s"API ($url) response $body")

      decode[List[ClusterInfoResponse]](body) match {
        case Left(err) =>
          logger.warn(s"Error when decoding ${err.getMessage}")
          null
        case Right(clusterInfo) =>
          clusterInfo.map(nodeInfo => getDagAddressFromPublicKey(nodeInfo.id, sp)).traverse(p => p)
      }
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching API: ${x.getMessage}")
        List.empty.pure[IO]
    }
  }

  def getValidatorNodesAddresses(metagraphL0NodeUrl: String, dataL1NodeUrl: String, securityProvider: SecurityProvider[IO]): (IO[List[Address]], IO[List[Address]]) = {
    logger.info(s"Starting to fetch validator nodes of env $metagraphL0NodeUrl and $dataL1NodeUrl")
    val l0ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(metagraphL0NodeUrl, securityProvider)
    val l1ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(dataL1NodeUrl, securityProvider)
    (l0ValidatorNodes, l1ValidatorNodes)
  }

}
