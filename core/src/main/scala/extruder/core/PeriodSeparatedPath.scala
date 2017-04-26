package extruder.core

trait PeriodSeparatedPath extends ResolutionCommon {
  override protected def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
}
