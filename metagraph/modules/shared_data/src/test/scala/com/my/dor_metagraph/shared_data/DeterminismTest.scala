package com.my.dor_metagraph.shared_data

import cats.syntax.option._
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.deserializers.Deserializers
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.auto._
import io.circe.parser.decode
import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import weaver.SimpleIOSuite

/**
  * Determinism is the load-bearing property of a metagraph: given the same (consensus-ordered)
  * inputs, every validator must recompute byte-identical calculated state and hashes, or consensus
  * forks. These tests pin that, the exact serialization round-trip, and BACKWARD COMPATIBILITY of
  * the calculated-state schema (old persisted state must still decode and keep its hash).
  */
object DeterminismTest extends SimpleIOSuite {

  private val addrA = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
  private val addrB = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")

  private def resp(a: Address): DorAPIResponse =
    DorAPIResponse(a.some, isInstalled = true, "Retail".some, none, none, none, none, none)

  private val epoch = EpochProgress(1440L)
  private def emptyState = DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(Map.empty))

  pureTest("combine is deterministic: identical ordered inputs produce byte-identical calculated state") {
    val uA = CheckInUpdate("idA", "sigA", 1669815076L, "hashA", resp(addrA).some)
    val uB = CheckInUpdate("idB", "sigB", 1669815077L, "hashB", resp(addrB).some)

    def run = combineDeviceCheckIn(combineDeviceCheckIn(emptyState, uA, addrA, epoch), uB, addrB, epoch)

    val r1 = run
    val r2 = run
    expect(r1.calculated.devices == r2.calculated.devices) &&
      expect(
        Serializers.serializeCalculatedState(r1.calculated)
          .sameElements(Serializers.serializeCalculatedState(r2.calculated))
      )
  }

  pureTest("calculated state round-trips exactly through serialize/deserialize on a populated state") {
    val analytics = AnalyticsBountyInformation(2880L, "team1", 123L, 10L, addrB.some).some
    val devices = Map(
      addrA -> DeviceInfo(1L, resp(addrA), 1440L, none, "pubA".some, "hashA".some),
      addrB -> DeviceInfo(2L, resp(addrB), 2880L, analytics, "pubB".some, "hashB".some)
    )
    val state = CheckInDataCalculatedState(devices, EpochProgress(99L).some)

    Deserializers.deserializeCalculatedState(Serializers.serializeCalculatedState(state)) match {
      case Right(decoded) => expect(decoded == state)
      case Left(e)        => failure(s"calculated state must round-trip: $e")
    }
  }

  pureTest("backward compatibility: pre-upgrade DeviceInfo JSON (no lastCheckInHash) decodes with None") {
    // DeviceInfo serialized before lastCheckInHash existed must still decode (field absent -> None),
    // and since None is dropped from JSON the device's hash is unchanged until it next checks in.
    val preUpgradeJson =
      s"""{"devices":{"${addrA.value.value}":{"lastCheckIn":1,"dorAPIResponse":{"rewardAddress":null,"isInstalled":true,"locationType":null,"billedAmountMonthly":null,"lastBillingId":null,"teamId":null,"billedAmount":null,"orgRewardAddress":null},"nextEpochProgressToReward":1440,"analyticsBountyInformation":null,"publicId":null}}}"""

    decode[CheckInDataCalculatedState](preUpgradeJson) match {
      case Right(state) =>
        expect(state.devices(addrA).lastCheckInHash.isEmpty) &&
          expect(state.lastEpochProgress.isEmpty) &&
          expect.eql(1440L, state.devices(addrA).nextEpochProgressToReward)
      case Left(e) => failure(s"pre-upgrade calculated-state JSON must decode: $e")
    }
  }
}
