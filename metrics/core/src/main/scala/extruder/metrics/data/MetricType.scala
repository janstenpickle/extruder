package extruder.metrics.data

trait MetricType {
  def name: String
}

object MetricType {
  case object Counter extends MetricType {
    override def name: String = "counter"
  }
  case object Gauge extends MetricType {
    override def name: String = "gauge"
  }
  case object Timer extends MetricType {
    override def name: String = "timer"
  }
}
