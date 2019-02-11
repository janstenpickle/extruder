package extruder.system

package object environment extends EnvironmentDecoder with EnvironmentDataSource with EnvironmentDecoderInstances {
  object decoder extends EnvironmentDecoder with EnvironmentDataSource
  object instances extends EnvironmentDecoderInstances
}
