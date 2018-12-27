package extruder.meta

import extruder.core.Settings

sealed trait Repr {
  def path: List[String]
  def `type`: String
  def required: Boolean
  def default: Option[String]
}

object Repr {
  case class StableRepr(path: List[String], `type`: String, required: Boolean, default: Option[String]) extends Repr
  case class UnionRepr(
    path: List[String],
    `type`: String,
    required: Boolean,
    default: Option[String],
    parentType: String
  ) extends Repr

  def asRepr[A](namespace: List[String], settings: Settings)(implicit ev: MetaInfo[A]): List[Repr] = {
    def unwrap(path: List[String], acc: List[Repr], metaInfo: BaseMetaInfo, required: Boolean, default: Option[String])(
      make: (List[String], String, Boolean, Option[String]) => Repr
    ): List[Repr] =
      metaInfo match {
        case product: Product[_] =>
          product.children.toList.flatMap {
            case (param, child) =>
              val newPath =
                if (settings.includeClassNameInPath) path ++ List(product.`type`, param)
                else path :+ param
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
