package com.my.dor_metagraph.shared_data.types

import com.my.dor_metagraph.shared_data.Utils.toTokenAmountFormat
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState, DataUpdate}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Balance
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.transaction.RewardTransaction

import java.time.Instant

object Types {
  val EpochProgress1Day: Long = 60 * 24

  val ModulusInstallationBounty: Long = 0
  val ModulusCommercialBounty: Long = 1
  val ModulusAnalyticsBounty: Long = 2

  val UndefinedTeamId: String = "Undefined"

  // Upper bound on DOR-reported billed amounts accepted into state. Server-sourced; drives analytics
  // payouts. Rejects negative/absurd values before they enter consensus. At 1e9 the worst-case
  // single-device reward is 1e9 * 25 DAG * 1e8 * 1.2 ≈ 3e18 datolites, ~3x below Long.MaxValue
  // (9.22e18); any residual overflow is a deterministic halt (Math.multiplyExact), never a silent
  // mint. Lower toward the real business maximum if known.
  val MaxBilledAmount: Long = 1_000_000_000L

  val MinimumCheckInSeconds: Long =
    Instant.parse("2023-09-01T00:00:00.00Z").toEpochMilli / 1000L

  val Collateral50K: Long = toTokenAmountFormat(50 * 1000)
  val Collateral100K: Long = toTokenAmountFormat(100 * 1000)
  val Collateral200K: Long = toTokenAmountFormat(200 * 1000)

  // Collateral reward multipliers as exact rationals instead of Double, so reward math is
  // integer-only and bit-identical on every validator (see Ratio below).
  val CollateralLessThan50KMultiplier: Ratio = Ratio(1L, 1L)
  val CollateralBetween50KAnd100KMultiplier: Ratio = Ratio(105L, 100L)
  val CollateralBetween100KAnd200KMultiplier: Ratio = Ratio(110L, 100L)
  val CollateralGreaterThan200KMultiplier: Ratio = Ratio(120L, 100L)

  /**
    * An exact, deterministic multiplier (numerator/denominator) for token math. Applies as
    * multiply-before-divide in BigInt so there is no floating-point rounding and no intermediate
    * Long overflow; the final conversion throws (deterministically, on every node) rather than
    * silently wrapping if the result somehow exceeds Long.
    */
  case class Ratio(numerator: Long, denominator: Long) {
    def applyTo(amount: Long): Long =
      (BigInt(amount) * BigInt(numerator) / BigInt(denominator)).bigInteger.longValueExact()
  }

  @derive(encoder, decoder)
  case class CheckInProof(
    id       : String,
    signature: String
  )

  @derive(encoder, decoder)
  case class CheckInStateUpdate(
    deviceId   : Address,
    dts        : Long,
    proof      : CheckInProof,
    checkInHash: String
  )

  @derive(encoder, decoder)
  case class DeviceInfo(
    lastCheckIn               : Long,
    dorAPIResponse            : DorAPIResponse,
    nextEpochProgressToReward : Long,
    analyticsBountyInformation: Option[AnalyticsBountyInformation],
    publicId                  : Option[String] = None,
    // Hash of the device's most recent accepted check-in. Used to reject a verbatim replay of a
    // signed check-in (the signature only covers this hash, so a replay reuses it). None encodes to
    // null and is dropped by deepDropNullValues, preserving pre-upgrade hashes until repopulated.
    lastCheckInHash           : Option[String] = None
  )

  @derive(encoder, decoder)
  case class DeviceCheckInWithSignature(
    cbor: String,
    hash: String,
    id  : String,
    sig : String
  )

  @derive(encoder, decoder)
  case class DeviceCheckInInfo(
    ac : List[Long],
    dts: Long,
    e  : List[List[Long]]
  )

  @derive(encoder, decoder)
  case class CheckInStateOnChain(
    updates: List[CheckInStateUpdate]
  ) extends DataOnChainState

  @derive(encoder, decoder)
  case class CheckInDataCalculatedState(
    devices          : Map[Address, DeviceInfo],
    // None encodes to null and is dropped by deepDropNullValues at every serialization
    // boundary (hash, disk, p2p), so pre-upgrade states keep their original hashes.
    // Populating it with Some changes the state hash: all source nodes must upgrade together.
    lastEpochProgress: Option[EpochProgress] = None
  ) extends DataCalculatedState

  @derive(encoder, decoder)
  case class CheckInUpdate(
    publicId           : String,
    signature          : String,
    dts                : Long,
    dtmCheckInHash     : String,
    maybeDorAPIResponse: Option[DorAPIResponse]
  ) extends DataUpdate

  /*
  These fields should match exactly the names of the fields returned from DOR API endpoint: `metagraph/:pub_id/check-in`
   */
  @derive(encoder, decoder)
  case class DorAPIResponse(
    rewardAddress      : Option[Address],
    isInstalled        : Boolean,
    locationType       : Option[String],
    billedAmountMonthly: Option[Long],
    lastBillingId      : Option[Long],
    teamId             : Option[String],
    billedAmount       : Option[Long],
    orgRewardAddress   : Option[Address]
  )

  @derive(encoder, decoder)
  case class CalculatedStateResponse(
    ordinal        : Long,
    calculatedState: CheckInDataCalculatedState
  )

  case class RewardTransactionsInformation(
    rewardTransactions: Map[Address, RewardTransaction],
    validatorsTaxes   : Long,
    lastBalances      : Map[Address, Balance]
  )

  case class RewardTransactionsAndValidatorsTaxes(
    rewardTransactions: List[RewardTransaction],
    validatorsTaxes   : Long
  )

  object RewardTransactionsAndValidatorsTaxes {
    def empty: RewardTransactionsAndValidatorsTaxes = RewardTransactionsAndValidatorsTaxes(List.empty, 0L)
  }

  @derive(encoder, decoder)
  case class AnalyticsBountyInformation(
    nextEpochProgressToRewardAnalytics: Long,
    teamId                            : String,
    lastBillingId                     : Long,
    billedAmount                      : Long,
    analyticsRewardAddress            : Option[Address]
  )
}