package com.my.dor_metagraph.l0.rewards.validators

import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.getEnv
import com.my.dor_metagraph.shared_data.external_apis.ClusterApi.getValidatorNodesAddresses
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

class ValidatorNodesAPI[F[_] : Async : SecurityProvider : Env] extends ValidatorNodes[F] {

  override def getValidatorNodes: F[(List[Address], List[Address])] =
    for {
      l0Url <- getEnv[F]("METAGRAPH_L0_NODE_URL")
      l1Url <- getEnv[F]("DATA_L1_NODE_URL")
      validatorsAddresses <- getValidatorNodesAddresses(l0Url, l1Url)
    } yield validatorsAddresses
}
