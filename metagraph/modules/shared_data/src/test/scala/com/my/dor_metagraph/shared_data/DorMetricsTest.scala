package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.metrics.DorMetrics
import weaver.SimpleIOSuite

object DorMetricsTest extends SimpleIOSuite {

  test("counters move by exact deltas, gauge is absolute, and render emits valid Prometheus") {
    for {
      acceptedBefore <- IO(DorMetrics.checkInsAccepted.get())
      datolitesBefore <- IO(DorMetrics.datolitesDistributed.get())
      _ <- DorMetrics.inc[IO](DorMetrics.checkInsAccepted)
      _ <- DorMetrics.addTo[IO](DorMetrics.datolitesDistributed, 1000L)
      _ <- DorMetrics.setDevicesInState[IO](42L)
      acceptedAfter <- IO(DorMetrics.checkInsAccepted.get())
      datolitesAfter <- IO(DorMetrics.datolitesDistributed.get())
    } yield {
      val lines = DorMetrics.renderPrometheus.linesIterator.toList
      expect.eql(acceptedBefore + 1L, acceptedAfter) &&
        expect.eql(datolitesBefore + 1000L, datolitesAfter) &&
        expect(lines.contains("# TYPE dor_checkins_accepted_total counter")) &&
        expect(lines.contains("# TYPE dor_devices_in_state gauge")) &&
        expect(lines.contains("dor_devices_in_state 42")) &&
        expect(lines.contains(s"dor_checkins_accepted_total $acceptedAfter")) &&
        // every sample emits exactly one HELP, one TYPE and one value line
        expect.eql(lines.count(_.startsWith("# HELP ")), lines.count(_.startsWith("# TYPE ")))
    }
  }
}
