package com.my.dor_metagraph.shared_data.bounties

import com.my.dor_metagraph.shared_data.types.Types.DorAPIResponse

trait Bounty {
  def getBountyRewardAmount(
    dorApiResponse: DorAPIResponse,
    epochModulus  : Long
  ): Double
}

