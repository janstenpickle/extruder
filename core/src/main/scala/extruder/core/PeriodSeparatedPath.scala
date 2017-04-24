package extruder.core

/**
  * Created by chris on 20/04/2017.
  */
trait PeriodSeparatedPath extends ResolutionCommon {
  override protected def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
}
