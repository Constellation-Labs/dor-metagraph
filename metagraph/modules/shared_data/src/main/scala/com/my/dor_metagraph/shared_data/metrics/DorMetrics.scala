package com.my.dor_metagraph.shared_data.metrics

import cats.effect.Sync

import java.util.concurrent.atomic.AtomicLong

/**
  * Per-node, per-process observability counters exposed in Prometheus text format at `/dor-metrics`
  * on each layer (data-L1 and metagraph-L0).
  *
  * IMPORTANT: these are NODE-LOCAL observability only. They are never read back into combine /
  * validation and are NOT part of calculated state — so they cannot influence consensus. Values are
  * approximate by design (e.g. `combine` may be re-executed across consensus rounds, and each node
  * counts independently); use them for trends/health, not for exact accounting.
  *
  * The framework's own `Metrics[F]` (Micrometer/Prometheus on `/metrics`) is not reachable from the
  * fixed-signature data-application lifecycle methods, hence this self-contained registry.
  */
object DorMetrics {

  // data-L1 ingress
  val checkInsReceived = new AtomicLong(0L)
  val checkInsAccepted = new AtomicLong(0L)
  val checkInsRejected = new AtomicLong(0L)

  // metagraph-L0 combine (consensus)
  val blocksCombined = new AtomicLong(0L)
  val checkInsCombined = new AtomicLong(0L)
  private val devicesInState = new AtomicLong(0L)

  // metagraph-L0 rewards
  val dailyRewardCycles = new AtomicLong(0L)
  val analyticsRewardCycles = new AtomicLong(0L)
  val datolitesDistributed = new AtomicLong(0L)
  val validatorTaxDistributed = new AtomicLong(0L)

  def setDevicesInState[F[_] : Sync](value: Long): F[Unit] = Sync[F].delay(devicesInState.set(value))

  /** Increment a counter as an effect (value discarded), so call sites stay referentially honest. */
  def inc[F[_] : Sync](counter: AtomicLong): F[Unit] = Sync[F].delay { counter.incrementAndGet(); () }

  /** Add to a counter as an effect. */
  def addTo[F[_] : Sync](counter: AtomicLong, delta: Long): F[Unit] = Sync[F].delay { counter.addAndGet(delta); () }

  private final case class Sample(name: String, help: String, kind: String, value: Long)

  private def samples: List[Sample] = List(
    Sample("dor_checkins_received_total", "Check-ins received at data-L1 ingress", "counter", checkInsReceived.get()),
    Sample("dor_checkins_accepted_total", "Check-ins that passed L1 validation", "counter", checkInsAccepted.get()),
    Sample("dor_checkins_rejected_total", "Check-ins rejected at L1 validation", "counter", checkInsRejected.get()),
    Sample("dor_blocks_combined_total", "Non-empty data blocks processed by combine at L0", "counter", blocksCombined.get()),
    Sample("dor_checkins_combined_total", "Check-ins processed by combine at L0", "counter", checkInsCombined.get()),
    Sample("dor_devices_in_state", "Devices currently tracked in calculated state", "gauge", devicesInState.get()),
    Sample("dor_daily_reward_cycles_total", "Daily reward distributions executed", "counter", dailyRewardCycles.get()),
    Sample("dor_analytics_reward_cycles_total", "Analytics reward distributions executed", "counter", analyticsRewardCycles.get()),
    Sample("dor_datolites_distributed_total", "Total datolites emitted as rewards", "counter", datolitesDistributed.get()),
    Sample("dor_validator_tax_distributed_total", "Total datolites distributed to validator nodes", "counter", validatorTaxDistributed.get())
  )

  /** Prometheus text exposition format (scrapeable as an additional target). */
  def renderPrometheus: String =
    samples
      .flatMap(s => List(s"# HELP ${s.name} ${s.help}", s"# TYPE ${s.name} ${s.kind}", s"${s.name} ${s.value}"))
      .mkString("\n") + "\n"
}
