package extruder.system
import extruder.map.MapDataSource

package object systemproperties
    extends SystemPropertiesEncoder
    with SystemPropertiesDecoder
    with MapDataSource
    with SystemPropertiesDecoderInstances
    with SystemPropertiesEncoderInstances {
  object decoder extends SystemPropertiesDecoder with MapDataSource
  object encoder extends SystemPropertiesEncoder with MapDataSource
  object instances extends SystemPropertiesDecoderInstances with SystemPropertiesEncoderInstances
}
