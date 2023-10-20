package com.my.dor_metagraph.shared_data.combiners

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import com.my.dor_metagraph.shared_data.external_apis.ClusterApi.getValidatorNodesAddresses
import com.my.dor_metagraph.shared_data.types.Types._
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

object ValidatorNodes {
  /**
   * Be sure to provide the variables: METAGRAPH_L0_NODE_URL and DATA_L1_NODE_URL on the startup of JAR
   * */
  def getValidatorNodes(currentEpochProgress: Long, currentStateOffChain: CheckInDataCalculatedState, securityProvider: SecurityProvider[IO]): (IO[List[Address]], IO[List[Address]]) = {
    val epochProgressModulus = currentEpochProgress % EPOCH_PROGRESS_1_DAY
    val l0ValidatorsNotFilledOrLessThan3 = currentStateOffChain.l0ValidatorNodesAddresses.isEmpty || currentStateOffChain.l0ValidatorNodesAddresses.size < 3
    val l1ValidatorsNotFilledOrLessThan3 = currentStateOffChain.l1ValidatorNodesAddresses.isEmpty || currentStateOffChain.l1ValidatorNodesAddresses.size < 3

    if (l0ValidatorsNotFilledOrLessThan3 || l1ValidatorsNotFilledOrLessThan3 || epochProgressModulus == 0L) {
      val metagraphL0NodeUrl = sys.env.getOrElse("METAGRAPH_L0_NODE_URL", throw new Exception("Error when getting METAGRAPH_L0_NODE_URL from ENV"))
      val dataL1NodeUrl = sys.env.getOrElse("DATA_L1_NODE_URL", throw new Exception("Error when getting DATA_L1_NODE_URL from ENV"))
      return getValidatorNodesAddresses(metagraphL0NodeUrl, dataL1NodeUrl, securityProvider)
    }

    (IO(currentStateOffChain.l0ValidatorNodesAddresses), IO(currentStateOffChain.l1ValidatorNodesAddresses))
  }
}
