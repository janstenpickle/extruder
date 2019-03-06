package extruder.core

import extruder.meta.{MetaInfo, ReprTable}

trait DataSource {
  type Sett <: Settings

  def defaultSettings: Sett

  def parameters[A: MetaInfo]: String = ReprTable.asTable(defaultSettings)

  def parameters[A: MetaInfo](settings: Sett): String = ReprTable.asTable(settings)

  def parameters[A: MetaInfo](namespace: List[String]): String = ReprTable.asTable(namespace, defaultSettings)

  def parameters[A: MetaInfo](namespace: List[String], settings: Sett): String = ReprTable.asTable(namespace, settings)
}
