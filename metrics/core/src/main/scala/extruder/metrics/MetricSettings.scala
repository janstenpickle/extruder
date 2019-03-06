package extruder.metrics

import extruder.core._
import extruder.metrics.data.MetricType

trait MetricSettings extends Settings {
  val mapEncoderSettings: Settings = new Settings {
    override val includeClassNameInPath: Boolean = false
    override def pathToString(path: List[String]): String = path.mkString(".")
  }

  override def pathToString(path: List[String]): String = path.map(snakeCaseTransformation).mkString(".")

  def defaultMetricType: MetricType = MetricType.Gauge
}
