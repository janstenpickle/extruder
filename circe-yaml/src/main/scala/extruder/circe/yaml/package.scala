package extruder.circe

package object yaml
    extends CirceYamlEncoder
    with CirceYamlFinalizeInstances
    with CirceEncoderInstances
    with CirceYamlDecoder
    with CirceYamlPrepareInstances
    with CirceDecoderInstances
    with CirceYamlDataSource {

  object instances
      extends CirceEncoderInstances
      with CirceYamlFinalizeInstances
      with CirceDecoderInstances
      with CirceYamlPrepareInstances

  object datasource extends CirceYamlDecoder with CirceYamlEncoder with CirceYamlDataSource

  object encoders
      extends CirceYamlEncoder
      with CirceEncoderInstances
      with CirceYamlFinalizeInstances
      with CirceYamlDataSource
  object decoders
      extends CirceYamlDecoder
      with CirceDecoderInstances
      with CirceYamlPrepareInstances
      with CirceYamlDataSource
}
