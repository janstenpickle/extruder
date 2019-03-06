package extruder

package object map
    extends MapEncoder
    with MapEncoderInstances
    with MapDecoder
    with MapDecoderInstances
    with MapDataSource {
  object instances extends MapEncoderInstances with MapDecoderInstances
  object datasource extends MapDecoder with MapEncoder with MapDataSource

  object encoders extends MapEncoder with MapEncoderInstances with MapDataSource
  object decoders extends MapDecoder with MapDecoderInstances with MapDataSource
}
