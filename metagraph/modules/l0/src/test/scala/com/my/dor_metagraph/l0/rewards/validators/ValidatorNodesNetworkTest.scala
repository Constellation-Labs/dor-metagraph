package com.my.dor_metagraph.l0.rewards.validators

import weaver.SimpleIOSuite

/**
  * The per-network seedlist selection drives WHO receives validator rewards, so a wrong mapping pays
  * the wrong network's validators. These pin the pure CL_APP_ENV -> network resolution.
  */
object ValidatorNodesNetworkTest extends SimpleIOSuite {

  pureTest("networkFor maps known CL_APP_ENV values (case/space-insensitive; dev -> testnet)") {
    expect(ValidatorNodes.networkFor(Some("mainnet")) == Right("mainnet")) &&
      expect(ValidatorNodes.networkFor(Some(" TESTNET ")) == Right("testnet")) &&
      expect(ValidatorNodes.networkFor(Some("integrationnet")) == Right("integrationnet")) &&
      expect(ValidatorNodes.networkFor(Some("dev")) == Right("testnet"))
  }

  pureTest("networkFor fails fast on unset or unknown CL_APP_ENV") {
    expect(ValidatorNodes.networkFor(None).isLeft) &&
      expect(ValidatorNodes.networkFor(Some("staging")).isLeft) &&
      expect(ValidatorNodes.networkFor(Some("")).isLeft)
  }
}
