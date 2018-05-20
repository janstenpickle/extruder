package extruder.metrics.dropwizard

import io.dropwizard.metrics5.Gauge
import io.dropwizard.metrics5.MetricRegistry.MetricSupplier

case class SimpleGauge(initial: Double) extends Gauge[Double] {
  private var _value = initial
  def set(value: Double): Unit = _value = value
  override def getValue: Double = _value
}

class SimpleGaugeSupplier extends MetricSupplier[Gauge[_]] {
  override def newMetric(): SimpleGauge = SimpleGauge(0.0)
}
