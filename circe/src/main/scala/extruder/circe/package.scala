package extruder

package object circe
    extends CirceEncoder
    with CirceEncoderInstances
    with CirceDecoder
    with CirceDecoderInstances
    with CirceDataSource {

  object instances extends CirceEncoderInstances with CirceDecoderInstances
  object datasource extends CirceDecoder with CirceEncoder with CirceDataSource

  object encoders extends CirceEncoder with CirceEncoderInstances with CirceDataSource
  object decoders extends CirceDecoder with CirceDecoderInstances with CirceDataSource
}
