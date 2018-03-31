package extruder

package object metrics {
  val snakeCaseTransformation: String => String =
    _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
}
