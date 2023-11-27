package com.my.dor_metagraph.shared_data.external_apis

import cats.effect.IO
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import com.my.dor_metagraph.shared_data.types.Types.ClusterInfoResponse
import io.circe.parser.decode
import org.slf4j.LoggerFactory
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import eu.timepit.refined.auto._

object ClusterApi {
  private val logger = LoggerFactory.getLogger("ClusterAPI")

  private def getValidatorNodesAddressesFromClusterInfo(url: String, sp: SecurityProvider[IO]): List[Address] = {
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
          clusterInfo.map(nodeInfo => getDagAddressFromPublicKey(nodeInfo.id, sp))
      }
    } catch {
      case x: Exception =>
        logger.warn(s"Error when fetching API: ${x.getMessage}")
        List.empty
    }
  }

  def getValidatorNodesAddresses(metagraphL0NodeUrl: String, dataL1NodeUrl: String, securityProvider: SecurityProvider[IO]): (List[Address], List[Address]) = {
    logger.info(s"Starting to fetch validator nodes of env $metagraphL0NodeUrl and $dataL1NodeUrl")
    val l0ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(metagraphL0NodeUrl, securityProvider)
    val l1ValidatorNodes = getValidatorNodesAddressesFromClusterInfo(dataL1NodeUrl, securityProvider)

    val validatorNodesL0 = if(l0ValidatorNodes.isEmpty){
      val addresses: List[Address] = List(Address("DAG0o6WSyvc7XfzujwJB1e25mfyzgXoLYDD6wqnk"), Address("DAG4YD6rkExLwYyAZzwjYJMxe36PAptKuUKq9uc7"), Address("DAG4nBD5J3Pr2uHgtS1sa16PqemHrwCcvjdR31Xe"))
      addresses
    } else{
      l0ValidatorNodes
    }
    val validatorNodesL1 = if (l1ValidatorNodes.isEmpty) {
      val addresses: List[Address] = List(Address("DAG5fqiGq9L5iLH5R5eV7gBjkucewrcaQ1jVnKYD"), Address("DAG6B5mBMoEu3Habtb2ts3QGUD2UquywrQSLSubU"), Address("DAG5uDuGhPuh4mQZGNLFCEcdy69txSF4iSfFbdWJ"))
      addresses
    } else {
      l1ValidatorNodes
    }

    (validatorNodesL0, validatorNodesL1)
  }
}