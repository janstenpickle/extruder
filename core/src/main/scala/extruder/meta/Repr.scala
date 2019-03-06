package extruder.meta

import extruder.core.Settings
import extruder.data.PathElement

sealed trait Repr {
  def path: List[PathElement]
  def `type`: String
  def required: Boolean
  def default: Option[String]
}

object Repr {
  case class StableRepr(path: List[PathElement], `type`: String, required: Boolean, default: Option[String])
      extends Repr
  case class UnionRepr(
    path: List[PathElement],
    `type`: String,
    required: Boolean,
    default: Option[String],
    parentType: String
  ) extends Repr

  def asRepr[A](namespace: List[PathElement], settings: Settings)(implicit ev: MetaInfo[A]): List[Repr] = {
    def unwrap(
      path: List[PathElement],
      acc: List[Repr],
      metaInfo: BaseMetaInfo,
      required: Boolean,
      default: Option[String]
    )(make: (List[PathElement], String, Boolean, Option[String]) => Repr): List[Repr] =
      metaInfo match {
        case product: Product[_] =>
          product.children.toList.flatMap {
            case (param, child) =>
              val newPath = path ++ List(PathElement.ClassName(product.`type`), PathElement.Standard(param))
              unwrap(newPath, acc, child.metaInfo, required, child.default)(make)
          }
        case union: Union[_] =>
          union.options.toList.flatMap(unwrap(path, acc, _, required, None)(UnionRepr(_, _, _, _, union.`type`)))
        case optional: Optional[_] => unwrap(path, acc, optional.value, required = false, default)(make)
        case any: BaseMetaInfo => make(path, any.`type`, required, default) :: acc
      }

    unwrap(namespace, List.empty, ev, required = true, None)(StableRepr)
  }
}
