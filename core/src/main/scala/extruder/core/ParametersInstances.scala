package extruder.core

import cats.data.NonEmptyList
import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, Default, Generic, HList, HNil, LabelledGeneric, Lazy, Poly1, Poly2, Witness}
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record.Keys

import scala.collection.TraversableOnce
import scala.reflect.runtime.universe.TypeTag

trait ParametersInstances extends Shows {

  implicit def objectParameters[T](implicit gen: Generic.Aux[T, HNil], tag: TypeTag[T]): Parameters[T] =
    Parameters(path =>
      List(StableRepr(path, required = true, ttToString[T], None))
    )

  implicit val cnilParameters: Parameters[CNil] = Parameters(_ => List.empty)

  implicit def cconsParameters[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K],
                                                               headParams: Lazy[Parameters[H]],
                                                               tailParams: Lazy[Parameters[T]]): Parameters[FieldType[K, H] :+: T] =
    Parameters(path =>
      headParams.value.eval(path) ++
      tailParams.value.eval(path)
    )

  implicit val cnilTypeName: TypeNames[CNil] = TypeNames(List.empty)

  implicit def cconsTypeNames[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K],
                                                              headType: TypeTag[H],
                                                              tailTypes: Lazy[TypeNames[T]]): TypeNames[FieldType[K, H] :+: T] =
    TypeNames(ttToString[H] :: tailTypes.value.values)

  implicit def unionParameters[T, V <: Coproduct](implicit gen: LabelledGeneric.Aux[T, V],
                                                  underlying: Lazy[Parameters[V]],
                                                  typeNames: Lazy[TypeNames[V]],
                                                  tag: TypeTag[T]): Parameters[T] =
    Parameters(path =>
      (for {
        types <- NonEmptyList.fromList(typeNames.value.values)
        impls <- NonEmptyList.fromList(underlying.value.eval(path).filterNot(_.path == path))
      } yield List(UnionRepr(path :+ ResolutionCommon.TypeKey, types, impls))).getOrElse(List.empty)
    )

  implicit def optionParameters[T](implicit getMeAConfigPath: Parameters[T]): Parameters[Option[T]] =
    Parameters(path => getMeAConfigPath.eval(path).map(_.updateRequired(false)))


  implicit def productParameters[T,
                                 DefaultOptsRepr <: HList,
                                 LGenRepr <: HList,
                                 KeysRepr <: HList,
                                 ConstRepr <: HList,
                                 ZipperRepr <: HList,
                                 PrefixZipperRepr <: HList,
                                 MapperRepr <: HList](implicit
                                                      defaultOpts: Default.AsOptions.Aux[T, DefaultOptsRepr],
                                                      lGen: LabelledGeneric.Aux[T, LGenRepr],
                                                      keys: Keys.Aux[LGenRepr, KeysRepr],
                                                      constMapper: ConstMapper.Aux[Seq[String], KeysRepr, ConstRepr],
                                                      prefixZipper: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
                                                      zipper: Zip.Aux[PrefixZipperRepr :: DefaultOptsRepr :: HNil, ZipperRepr],
                                                      lazyMapper: Lazy[Mapper.Aux[readParams.type, ZipperRepr, MapperRepr]],
                                                      rightFolder: RightFolder.Aux[MapperRepr, List[ParamRepr], folder.type, List[ParamRepr]],
                                                      tag: TypeTag[T]): Parameters[T] = Parameters { path =>
    lazy val className: String = ttToString[T]
    val keyNames = Keys[LGenRepr].apply()

    keyNames.zip(keyNames.mapConst(path :+ className)).zip(Default.AsOptions[T].apply()).map(readParams)(lazyMapper.value).foldRight(List.empty[ParamRepr])(folder)
  }

  object readParams extends Poly1 {
    implicit def casePrimitive[A <: Symbol, B](implicit show: Lazy[Show[B]], tag: TypeTag[B]): Case.Aux[((A, Seq[String]), Option[B]), List[ParamRepr]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => List(StableRepr(prefix :+ key.name, default.fold(true)(_ => false), ttToString[B], default.map(show.value.show))) }

    implicit def caseOptionalPrimitive[A <: Symbol, B](implicit show: Lazy[Show[B]], tag: TypeTag[B]): Case.Aux[((A, Seq[String]), Option[Option[B]]), List[ParamRepr]] =
      at[((A, Seq[String]), Option[Option[B]])]{ case ((key, prefix), default) => List(StableRepr(prefix :+ key.name, required = false, ttToString[B], default.flatten.map(show.value.show))) }

    implicit def caseProduct[A <: Symbol, B](implicit show: Lazy[Parameters[B]]): Case.Aux[((A, Seq[String]), Option[B]), List[ParamRepr]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => show.value.eval(prefix :+ key.name) }

    implicit def caseTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit show: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[F[B]]), List[ParamRepr]] =
      at[((A, Seq[String]), Option[F[B]])]{ case ((key, prefix), default) => List(StableRepr(prefix :+ key.name, default.fold(true)(_ => false), s"${ttToString[F[B]]}[${ttToString[B]}]",  default.map(_.map(show.value.show).mkString(",")))) }

    implicit def caseOptionTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit show: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[Option[F[B]]]), List[ParamRepr]] =
      at[((A, Seq[String]), Option[Option[F[B]]])]{ case ((key, prefix), default) => List(StableRepr(prefix :+ key.name, required = false, s"${ttToString[F[B]]}[${ttToString[B]}]", default.flatten.map(_.map(show.value.show).mkString(",")))) }

  }

  object folder extends Poly2 {
    implicit val caseHList: Case.Aux[List[ParamRepr], List[ParamRepr], List[ParamRepr]] =
      at[List[ParamRepr], List[ParamRepr]]((a, b) => a ++ b)
  }

  def ttToString[A](implicit tag: TypeTag[A]): String = tag.tpe.typeSymbol.name.toString
}

case class Parameters[T](eval: Seq[String] => List[ParamRepr])

object Parameters extends ParametersInstances {
  def apply[T](implicit params: Parameters[T]): Parameters[T] = params
}

case class TypeNames[T](values: List[String])

object TypeNames extends ParametersInstances {
  def apply[T](implicit typeNames: TypeNames[T]): TypeNames[T] = typeNames
}

trait ParamRepr {
  def path: Seq[String]
  def required: Boolean
  def default: Option[String]
  def updateRequired(req: Boolean): ParamRepr
}

case class StableRepr(path: Seq[String], required: Boolean, `type`: String, default: Option[String]) extends ParamRepr {
  override def updateRequired(req: Boolean): StableRepr = copy(required = req)
}

case class UnionRepr(path: Seq[String], types: NonEmptyList[String], reprs: NonEmptyList[ParamRepr]) extends ParamRepr {
  override val required: Boolean = true
  override val default: Option[String] = None
  override def updateRequired(req: Boolean): UnionRepr = this
}
