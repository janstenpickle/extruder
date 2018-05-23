package extruder.metrics.data

case class Metrics(statuses: Map[List[String], String], metrics: Map[MetricKey, Numbers])

object Metrics {
  def apply(metrics: Map[MetricKey, Numbers]): Metrics = Metrics(Map.empty, metrics)
  def single(key: MetricKey, value: Numbers): Metrics = Metrics(Map(key -> value))
  def status(key: List[String], value: String): Metrics = Metrics.statuses(Map(key -> value))
  def statuses(statuses: Map[List[String], String]): Metrics = Metrics(statuses, Map.empty)
}
