package extruder.metrics.prometheus

package object registry
    extends PrometheusRegistryEncoder
    with PrometheusRegistryDataSource
    with PrometheusRegistryEncoderInstances {
  object encoder extends PrometheusRegistryEncoder with PrometheusRegistryDataSource
  object instances extends PrometheusRegistryEncoderInstances
}
