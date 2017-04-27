package extruder.core

import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, Default, Generic, HList, HNil, LabelledGeneric, Lazy, Poly1, Poly2, Witness}
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record.Keys

import scala.collection.TraversableOnce
import scala.reflect.runtime.universe.TypeTag

trait ParametersInstances extends Shows {

  implicit def objectParameters[T](implicit gen: Generic.Aux[T, HNil], tag: TypeTag[T]): Parameters[T] =
    Parameters(path =>
      Map((path, (None, ttToString[T], true)))
    )

  implicit val cnilParameters: Parameters[CNil] = Parameters(_ => Map.empty)

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
      Map((path :+ "type", (Some(typeNames.value.values.mkString("|")), "String", true))) ++
      underlying.value.eval(path).filterNot(_._1 == path)
    )

  implicit def optionParameters[T](implicit getMeAConfigPath: Parameters[T]): Parameters[Option[T]] =
    Parameters((path => getMeAConfigPath.eval(path).mapValues { case (default, tpe, _) => (default, tpe, false) }))


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
                                                      rightFolder: RightFolder.Aux[MapperRepr, ParametersMap, folder.type, ParametersMap],
                                                      tag: TypeTag[T]): Parameters[T] = Parameters { path =>
    lazy val className: String = ttToString[T]
    val keyNames = Keys[LGenRepr].apply()

    keyNames.zip(keyNames.mapConst(path :+ className)).zip(Default.AsOptions[T].apply()).map(readParams)(lazyMapper.value).foldRight(Map.empty[Seq[String], (Option[String], String, Boolean)])(folder)
  }

  object readParams extends Poly1 {
    implicit def casePrimitive[A <: Symbol, B](implicit show: Lazy[Show[B]], tag: TypeTag[B]): Case.Aux[((A, Seq[String]), Option[B]), ParametersMap] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => Map(((prefix :+ key.name), (default.map(show.value.show), ttToString[B], default.fold(true)(_ => false)))) }

    implicit def caseOptionalPrimitive[A <: Symbol, B](implicit show: Lazy[Show[B]], tag: TypeTag[B]): Case.Aux[((A, Seq[String]), Option[Option[B]]), ParametersMap] =
      at[((A, Seq[String]), Option[Option[B]])]{ case ((key, prefix), default) => Map(((prefix :+ key.name), (default.flatten.map(show.value.show), ttToString[B], false))) }

    implicit def caseProduct[A <: Symbol, B](implicit show: Lazy[Parameters[B]]): Case.Aux[((A, Seq[String]), Option[B]), ParametersMap] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => show.value.eval(prefix :+ key.name) }

    implicit def caseTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit show: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[F[B]]), ParametersMap] =
      at[((A, Seq[String]), Option[F[B]])]{ case ((key, prefix), default) => Map(((prefix :+ key.name), (default.map(_.map(show.value.show).mkString(",")), s"${ttToString[F[B]]}[${ttToString[B]}]", default.fold(true)(_ => false)))) }

    implicit def caseOptionTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit show: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[Option[F[B]]]), ParametersMap] =
      at[((A, Seq[String]), Option[Option[F[B]]])]{ case ((key, prefix), default) => Map(((prefix :+ key.name), (default.flatten.map(_.map(show.value.show).mkString(",")), s"${ttToString[F[B]]}[${ttToString[B]}]", false))) }

  }

  object folder extends Poly2 {
    implicit val caseHList: Case.Aux[ParametersMap, ParametersMap, ParametersMap] =
      at[ParametersMap, ParametersMap]((a, b) => a ++ b)
  }

  def ttToString[A](implicit tag: TypeTag[A]): String = tag.tpe.typeSymbol.name.toString
}

case class Parameters[T](eval: Seq[String] => ParametersMap)

object Parameters extends ParametersInstances {
  def apply[T](implicit params: Parameters[T]): Parameters[T] = params
}

case class TypeNames[T](values: List[String])

object TypeNames extends ParametersInstances {
  def apply[T](implicit typeNames: TypeNames[T]): TypeNames[T] = typeNames
}