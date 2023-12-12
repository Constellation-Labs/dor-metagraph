package com.my.dor_metagraph.l0.rewards.validators

import cats.effect.Async
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.external_apis.ClusterApi.getValidatorNodesAddresses
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider

class ValidatorNodesAPI[F[_] : Async : SecurityProvider](
  l0UrlF: F[String],
  l1UrlF: F[String]
) extends ValidatorNodes[F] {

  override def getValidatorNodes: F[(List[Address], List[Address])] =
    for {
      l0Url <- l0UrlF
      l1Url <- l1UrlF
      validatorsAddresses <- getValidatorNodesAddresses(l0Url, l1Url)
    } yield validatorsAddresses
}
