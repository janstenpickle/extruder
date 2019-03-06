package extruder

package object typesafe
    extends TypesafeConfigEncoder
    with TypesafeConfigEncoderInstances
    with TypesafeConfigDecoder
    with TypesafeConfigDecoderInstances
    with TypesafeConfigDataSource {
  object instances extends TypesafeConfigEncoderInstances with TypesafeConfigDecoderInstances
  object datasource extends TypesafeConfigDecoder with TypesafeConfigEncoder with TypesafeConfigDataSource

  object encoders extends TypesafeConfigEncoder with TypesafeConfigEncoderInstances with TypesafeConfigDataSource
  object decoders extends TypesafeConfigDecoder with TypesafeConfigDecoderInstances with TypesafeConfigDataSource
}
