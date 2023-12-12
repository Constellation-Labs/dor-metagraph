package com.my.dor_metagraph.l0.rewards.validators

import org.tessellation.schema.address.Address

trait ValidatorNodes[F[_]] {
  def getValidatorNodes: F[(List[Address], List[Address])]
}
