package com.my.dor_metagraph.shared_data

import com.my.dor_metagraph.shared_data.Types.ClusterInfoResponse
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address

object ClusterApi {
  private val clusterApi = ClusterApi()

  def getValidatorNodesAddresses(environment: String): (List[Address], List[Address]) = {
    clusterApi.getValidatorNodesAddresses(environment)
  }
}

case class ClusterApi() {
  private val logger = LoggerFactory.getLogger(classOf[ClusterApi])

  private val TESTNET_BASE_URLS: (String, String) = ("http://34.212.38.215:7000/cluster/info", "http://34.212.38.215:9000/cluster/info")
  private val INTEGRATIONNET_BASE_URLS: (String, String) = ("http://54.218.46.24:7000/cluster/info", "http://54.218.46.24:9000/cluster/info")
  private val LOCAL_DEV_BASE_URLS: (String, String) = ("http://host.docker.internal:9400/cluster/info", "http://host.docker.internal:8000/cluster/info")

  private def getValidatorNodesAddressesFromClusterInfo(url: String): List[Address] = {
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
          clusterInfo.map(nodeInfo => getDagAddressFromPublicKey(nodeInfo.id))
      }
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching API: ${x.getMessage}")
        List.empty
    }
  }

  def getValidatorNodesAddresses(environment: String): (List[Address], List[Address]) = {
    val baseUrls = environment match {
      case "testnet" => TESTNET_BASE_URLS
      case "integrationnet" => INTEGRATIONNET_BASE_URLS
      case _ => LOCAL_DEV_BASE_URLS
    }
    logger.info(s"Starting to fetch validator nodes of env ${baseUrls._1} and ${baseUrls._2}")
    val l0ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(s"${baseUrls._1}")
    val l1ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(s"${baseUrls._2}")
    (l0ValidatorNodes, l1ValidatorNodes)
  }

}
