package extruder.metrics.dropwizard

package object dimensional
    extends DropwizardDimensionalEncoder
    with DropwizardDimensionalDataSource
    with DropwizardDimensionalEncoderInstances {
  object encoder extends DropwizardDimensionalEncoder with DropwizardDimensionalDataSource
  object instances extends DropwizardDimensionalEncoderInstances
}
