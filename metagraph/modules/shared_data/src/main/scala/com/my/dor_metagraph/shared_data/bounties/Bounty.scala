package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

trait Bounty {
  // Reward amount in whole DAG (integer). Converted to datolites downstream via toTokenAmountFormat.
  // Integer-only so the reward pipeline carries no Double and stays deterministic across nodes.
  def getBountyRewardAmount(
    dorApiResponse: DorAPIResponse,
    epochModulus  : Long
  ): Long
}

