package com.my.dor_metagraph.shared_data

import cats.effect.IO
import cats.syntax.option._
import com.my.dor_metagraph.shared_data.LifecycleSharedFunctions.getCurrentEpochProgress
import com.my.dor_metagraph.shared_data.calculated_state.CalculatedStateService
import com.my.dor_metagraph.shared_data.combiners.DeviceCheckIn.combineDeviceCheckIn
import com.my.dor_metagraph.shared_data.serializers.Serializers
import com.my.dor_metagraph.shared_data.types.Types._
import eu.timepit.refined.auto._
import io.circe.parser.decode
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import weaver.SimpleIOSuite

import java.nio.charset.StandardCharsets

object LifecycleSharedFunctionsTest extends SimpleIOSuite {

  private val address = Address("DAG0DQPuvVThrVnz66S4V6cocrtpg59oesAWyRMb")

  private val contextWithoutLastSnapshot: L0NodeContext[IO] = new L0NodeContext[IO] {
    def getLastCurrencySnapshot = IO.pure(none)
    def getCurrencySnapshot(ordinal: SnapshotOrdinal) = IO.pure(none)
    def getLastCurrencySnapshotCombined = IO.pure(none)
    def getLastSynchronizedGlobalSnapshot = IO.pure(none)
    def getLastSynchronizedGlobalSnapshotCombined = IO.pure(none)
    def getLastSynchronizedAllowSpends = IO.pure(none)
    def getLastSynchronizedTokenLocks = IO.pure(none)
    def securityProvider = ???
    def getCurrencyId = ???
    def getMetagraphL0Seedlist = none
  }

  test("getCurrentEpochProgress falls back to lastEpochProgress stored in calculated state") {
    implicit val context: L0NodeContext[IO] = contextWithoutLastSnapshot
    val storedEpoch = EpochProgress(1708273L)
    val state = CheckInDataCalculatedState(Map.empty, storedEpoch.some)

    getCurrentEpochProgress[IO](state).map(resolved => expect(resolved == storedEpoch))
  }

  test("getCurrentEpochProgress raises when neither context nor stored epoch is available") {
    implicit val context: L0NodeContext[IO] = contextWithoutLastSnapshot
    val state = CheckInDataCalculatedState(Map.empty)

    getCurrentEpochProgress[IO](state).attempt.map {
      case Left(e)  => expect(e.getMessage.contains("lastCurrencySnapshot not found"))
      case Right(_) => failure("expected getCurrentEpochProgress to raise")
    }
  }

  pureTest("pre-upgrade calculated state JSON decodes with lastEpochProgress=None") {
    val oldShapeJson =
      s"""{"devices":{"${address.value.value}":{"lastCheckIn":1,"dorAPIResponse":{"rewardAddress":null,"isInstalled":true,"locationType":null,"billedAmountMonthly":null,"lastBillingId":null,"teamId":null,"billedAmount":null,"orgRewardAddress":null},"nextEpochProgressToReward":1440,"analyticsBountyInformation":null,"publicId":null}}}"""

    decode[CheckInDataCalculatedState](oldShapeJson) match {
      case Right(state) =>
        expect(state.lastEpochProgress.isEmpty) &&
          expect.eql(1440L, state.devices(address).nextEpochProgressToReward)
      case Left(e) => failure(s"old-shape JSON must decode: $e")
    }
  }

  pureTest("serialized bytes are unchanged while lastEpochProgress is None") {
    val state = CheckInDataCalculatedState(Map.empty)
    val oldShapeBytes = """{"devices":{}}""".getBytes(StandardCharsets.UTF_8)

    expect(Serializers.serializeCalculatedState(state).sameElements(oldShapeBytes))
  }

  test("hash is unchanged while lastEpochProgress is None and changes once populated") {
    val state = CheckInDataCalculatedState(Map.empty)
    CalculatedStateService.make[IO].flatMap { service =>
      for {
        hashNone <- service.hashCalculatedState(state)
        hashOldShape <- service.hashCalculatedState(state.copy(lastEpochProgress = none))
        hashSome <- service.hashCalculatedState(state.copy(lastEpochProgress = EpochProgress(1L).some))
      } yield expect(hashNone == hashOldShape) && expect(hashNone != hashSome)
    }
  }

  pureTest("combineDeviceCheckIn skips updates without a DOR API response") {
    val oldState = DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(Map.empty))
    val checkInWithoutApiResponse = CheckInUpdate("123", "456", 1669815076L, "123", none)

    val result = combineDeviceCheckIn(oldState, checkInWithoutApiResponse, address, EpochProgress(1440L))

    expect(result == oldState)
  }

  pureTest("combineDeviceCheckIn preserves lastEpochProgress") {
    val storedEpoch = EpochProgress(1708273L)
    val oldState = DataState(CheckInStateOnChain(List.empty), CheckInDataCalculatedState(Map.empty, storedEpoch.some))
    val deviceInfoAPIResponse = DorAPIResponse(address.some, isInstalled = true, "Retail".some, none, none, none, none, none)
    val checkInRaw = CheckInUpdate("123", "456", 1669815076L, "123", deviceInfoAPIResponse.some)

    val result = combineDeviceCheckIn(oldState, checkInRaw, address, EpochProgress(1440L))

    expect(result.calculated.lastEpochProgress.contains(storedEpoch))
  }
}
