package extruder

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue}
import shapeless.{:+:, CNil}

package object typesafe {
  type ConfigTypes = String :+: ConfigValue :+: ConfigList :+: ConfigObject :+: List[String] :+: CNil
  type ConfigMap = Map[String, ConfigTypes]
}
