package com.my.dor_metagraph.l0.rewards

import cats.effect.Async
import com.my.dor_metagraph.shared_data.external_apis.ClusterApi.getValidatorNodesAddresses
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

object ValidatorNodes {
  /**
   * Be sure to provide the variables: METAGRAPH_L0_NODE_URL and DATA_L1_NODE_URL on the startup of JAR
   * */
  def getValidatorNodes[F[_]: Async: SecurityProvider]: F[(List[Address], List[Address])] = {
      val metagraphL0NodeUrl = sys.env.getOrElse("METAGRAPH_L0_NODE_URL", throw new Exception("Error when getting METAGRAPH_L0_NODE_URL from ENV"))
      val dataL1NodeUrl = sys.env.getOrElse("DATA_L1_NODE_URL", throw new Exception("Error when getting DATA_L1_NODE_URL from ENV"))
      getValidatorNodesAddresses(metagraphL0NodeUrl, dataL1NodeUrl)
  }
}
