package extruder.core

import scala.collection.JavaConverters._

class SystemPropertiesResolvers extends MapResolvers(System.getProperties.asScala.toMap)

object SystemPropertiesResolvers extends SystemPropertiesResolvers