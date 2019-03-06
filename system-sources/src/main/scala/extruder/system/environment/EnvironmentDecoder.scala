package extruder.system.environment
import extruder.core.Decode
import extruder.data.Validation

trait EnvironmentDecoder extends Decode { self: EnvironmentDataSource =>
  override type InputData = java.util.Map[String, String]
  override type DecodeData = Map[String, String]
  override type DecodeDefault[A] = Validation[A]
}
