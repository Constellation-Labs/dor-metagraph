package com.my.dor_metagraph.shared_data

import cats.effect.IO
import com.my.dor_metagraph.shared_data.metrics.DorMetrics
import weaver.SimpleIOSuite

object DorMetricsTest extends SimpleIOSuite {

  test("renderPrometheus emits HELP/TYPE/value lines and reflects counter state") {
    for {
      _ <- DorMetrics.inc[IO](DorMetrics.checkInsAccepted)
      _ <- DorMetrics.setDevicesInState[IO](42L)
    } yield {
      val lines = DorMetrics.renderPrometheus.linesIterator.toList
      expect(lines.contains("# TYPE dor_checkins_accepted_total counter")) &&
        expect(lines.contains("# TYPE dor_devices_in_state gauge")) &&
        expect(lines.exists(_.startsWith("dor_checkins_accepted_total "))) &&
        expect(lines.contains("dor_devices_in_state 42"))
    }
  }
}
