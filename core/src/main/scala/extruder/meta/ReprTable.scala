package extruder.meta

import extruder.core.Settings
import extruder.data.PathElement
import extruder.meta.Repr._

object ReprTable {
  private val KeyCol = "Key"
  private val RequiredCol = "Required"
  private val TypeCol = "Type"
  private val DefaultCol = "Default"
  private val ParentCol = "Parent Trait"

  private def convertRequired(required: Boolean): String =
    if (required) "Y" else "N"

  private def colSep(length: Int): String =
    List.fill(length + 2)('-').mkString

  def maxLength(colName: String, selector: Repr => String, reprs: List[Repr]): Int =
    (colName.length :: reprs.map(p => selector(p).length)).max

  val typesToString: Repr => String = {
    case union: UnionRepr => union.parentType
    case _: Any => ""
  }

  def asTable[A: MetaInfo](settings: Settings): String = asTable(List.empty, settings)

  def asTable[A: MetaInfo](namespace: List[String], settings: Settings): String = {
    val reprs = asRepr[A](namespace.map(PathElement.Standard), settings)
    val maxKeyLength = maxLength(KeyCol, p => settings.pathElementListToString(p.path), reprs)
    val maxRequiredLength = maxLength(RequiredCol, p => convertRequired(p.required), reprs)
    val maxTypeLength = maxLength(TypeCol, _.`type`, reprs)
    val maxDefaultLength = maxLength(DefaultCol, _.default.getOrElse(""), reprs)
    val maxAllowedValuesLength = maxLength(ParentCol, typesToString, reprs)

    val leftAlignFormat: String =
      s"| %-${maxKeyLength}s | %-${maxRequiredLength}s | %-${maxTypeLength}s | %-${maxDefaultLength}s | %-${maxAllowedValuesLength}s |%n"
    val separator: String =
      s"+${colSep(maxKeyLength)}+${colSep(maxRequiredLength)}+${colSep(maxTypeLength)}+${colSep(maxDefaultLength)}+${colSep(maxAllowedValuesLength)}+%n"

    val header = separator + leftAlignFormat.format(KeyCol, RequiredCol, TypeCol, DefaultCol, ParentCol) + separator
    val rows = reprs.foldLeft("") { (acc, p) =>
      acc + leftAlignFormat
        .format(
          settings.pathElementListToString(p.path),
          convertRequired(p.required),
          p.`type`,
          p.default.getOrElse(""),
          typesToString(p)
        )
    }
    (header + rows + separator).format()
  }
}
