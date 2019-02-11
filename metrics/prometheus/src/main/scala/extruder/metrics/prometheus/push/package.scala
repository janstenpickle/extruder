package extruder.metrics.prometheus

package object push extends PrometheusPushEncoder with PrometheusPushDataSource with PrometheusPushEncoderInstances {
  object encoder extends PrometheusPushEncoder with PrometheusPushDataSource
  object instances extends PrometheusPushEncoderInstances
}
