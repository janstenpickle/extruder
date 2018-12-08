package extruder.metrics.dropwizard

package object keyed
    extends DropwizardKeyedEncoder
    with DropwizardKeyedDataSource
    with DropwizardKeyedEncoderInstances {
  object encoder extends DropwizardKeyedEncoder with DropwizardKeyedDataSource
  object instances extends DropwizardKeyedEncoderInstances
}
