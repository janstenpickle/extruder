package extruder.core

import cats.data.NonEmptyList
import shapeless.labelled.FieldType
import shapeless._
import shapeless.ops.adjoin.Adjoin
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._

import scala.collection.TraversableOnce
import scala.reflect.runtime.universe.TypeTag

trait ParametersInstances extends Shows {

  implicit def objectParameters[T](implicit gen: Generic.Aux[T, HNil], tag: TypeTag[T]): NewParameters.Aux[T, HNil] = new NewParameters[T] {
    override type Repr = HNil
    override def eval(path: Seq[String]): HNil = HNil
  }

  implicit val cnilParameters: NewParameters.Aux[CNil, HNil] = new NewParameters[CNil] {
    override type Repr = HNil
    override def eval(path: Seq[String]): HNil = HNil
  }

  implicit def cconsParameters[H, T <: Coproduct, HeadRepr <: HList, TailRepr <: HList, Out <: HList](implicit
                                                               headParams: Lazy[NewParameters.Aux[H, HeadRepr]],
                                                               tailParams: Lazy[NewParameters.Aux[T, TailRepr]],
                                                                                                     adjoin: Adjoin.Aux[HeadRepr :: TailRepr, Out]): NewParameters.Aux[H :+: T, Out] = new NewParameters[H :+: T] {
    override type Repr = Out
    override def eval(path: Seq[String]): Out = (headParams.value.eval(path) :: tailParams.value.eval(path)).adjoined
  }

//  implicit def optionShit[ T]: NewParameters.Aux[None.type :+: Some[T] :+: CNil, Unit :: HNil] =
//    new NewParameters[None.type :+: Some[T] :+: CNil] {
//      override type Repr = Unit :: HNil
//
//      override def eval(path: Seq[String]): Unit :: HNil = () :: HNil
//    }

  implicit val cnilTypeName: TypeNames[CNil] = TypeNames(List.empty)

  implicit def cconsTypeNames[H, T <: Coproduct](implicit
                                                              headType: TypeTag[H],
                                                              tailTypes: Lazy[TypeNames[T]]): TypeNames[H :+: T] =
    TypeNames(ttToString[H] :: tailTypes.value.values)



  implicit def unionParameters[T, V <: Coproduct, VRepr <: HList](implicit gen: Generic.Aux[T, V],
                                                                  underlying: Lazy[NewParameters.Aux[V, VRepr]],
                                                                  typeNames: Lazy[TypeNames[V]],
                                                                  tag: TypeTag[T]): NewParameters.Aux[T, NewParameter[T] :: VRepr] =
    new NewParameters[T] {
      override type Repr = NewParameter[T] :: VRepr

      override def eval(path: Seq[String]): NewParameter[T] :: VRepr =
        NewUnionParameter[T](path :+ ResolutionCommon.TypeKey, ttToString[T], typeNames.value.values) :: underlying.value.eval(path)
    }


  implicit def primitive[T](implicit shows: Show[T]): NewParameters.Aux[T, String :: HNil] = new NewParameters[T] {
    override type Repr = String :: HNil
    override def eval(path: Seq[String]): ::[String, HNil] = "cunt" :: HNil
  }



//  implicit def optionalProductParameters[T, URepr <: HList, OutRepr <: HList](implicit params: NewParameters.Aux[T, URepr],
//                                                                              mapper: Mapper.Aux[shit.type, URepr, OutRepr]): NewParameters.Aux[Option[T], OutRepr] =
//    new NewParameters[Option[T]] {
//      override type Repr = OutRepr
//      override def eval(path: Seq[String]): OutRepr = params.eval(path).map(shit)
//    }
//
//  object shit extends Poly1 {
//    implicit def caseAny[T]: Case.Aux[NewParameter[T], NewParameter[T]] = at[NewParameter[T]](_.updateRequired(false))
//  }
    //Parameters(path => getMeAConfigPath.eval(path).map(_.updateRequired(false)))

  implicit def productParameters[T,
                                 DefaultOptsRepr <: HList,
                                 FilteredRepr <: HList,
                                 LGenRepr <: HList,
                                 KeysRepr <: HList,
                                 ConstRepr <: HList,
                                 ZipperRepr <: HList,
                                 PrefixZipperRepr <: HList,
                                 MapperRepr <: HList,
                                 OutRepr <: HList](implicit
                                                      defaultOpts: Default.AsOptions.Aux[T, DefaultOptsRepr],
                                                      lGen: LabelledGeneric.Aux[T, LGenRepr],
                                                      keys: Keys.Aux[LGenRepr, KeysRepr],
                                                      constMapper: ConstMapper.Aux[Seq[String], KeysRepr, ConstRepr],
                                                      prefixZipper: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
                                                      zipper: Zip.Aux[PrefixZipperRepr :: DefaultOptsRepr :: HNil, ZipperRepr],
                                                      lazyMapper: Lazy[Mapper.Aux[readParams.type, ZipperRepr, MapperRepr]],
                                                      adjoin: Adjoin.Aux[MapperRepr, OutRepr],
                                                      tag: TypeTag[T]): NewParameters.Aux[T, OutRepr] = new NewParameters[T] {
    override type Repr = OutRepr

    lazy val className: String = ttToString[T]
    val keyNames = Keys[LGenRepr].apply()

    override def eval(path: Seq[String]): OutRepr =
      keyNames
        .zip(keyNames.mapConst(path :+ className))
        .zip(Default.AsOptions[T].apply())
        .map(readParams)(lazyMapper.value)
        .adjoined
  }



  object readParams extends Poly1 {
    implicit def casePrimitive[A <: Symbol, B](implicit tag: TypeTag[B], shows: Show[B]): Case.Aux[((A, Seq[String]), Option[B]), NewProductParameter[B] :: HNil] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) =>
        NewProductParameter[B](prefix :+ key.name, default.isEmpty, ttToString[B], default) :: HNil
      }

//    implicit def casePrimitive[A <: Symbol, B](implicit show: Lazy[Show[B]], tag: TypeTag[B]): Case.Aux[((A, Seq[String]), Option[B]), List[Parameter]] =
//      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => List(ProductParameter(prefix :+ key.name, default.fold(true)(_ => false), ttToString[B], default.map(show.value.show))) }

    implicit def caseOptionalPrimitive[A <: Symbol, B](implicit tag: TypeTag[B], shows: Show[B]): Case.Aux[((A, Seq[String]), Option[Option[B]]), NewProductParameter[Option[B]] :: HNil] =
      at[((A, Seq[String]), Option[Option[B]])]{ case ((key, prefix), default) =>
        NewProductParameter[Option[B]](prefix :+ key.name, required = false, ttToString[B], default) :: HNil
      }
//
    implicit def caseProduct[A <: Symbol, B, Repr <: HList](implicit show: Lazy[NewParameters.Aux[B, Repr]]): Case.Aux[((A, Seq[String]), Option[B]), Repr] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => show.value.eval(prefix :+ key.name) }
////
//    implicit def caseTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit show: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[F[B]]), List[Parameter]] =
//      at[((A, Seq[String]), Option[F[B]])]{ case ((key, prefix), default) => List(ProductParameter(prefix :+ key.name, default.fold(true)(_ => false), traversableTtToString[B, F],  default.map(_.map(show.value.show).mkString(",")))) }
//
//    implicit def caseOptionTraversable[A <: Symbol, B, F[C] <: TraversableOnce[C]](implicit shows: Lazy[Show[B]], tag: TypeTag[B], traversableTag: TypeTag[F[B]]): Case.Aux[((A, Seq[String]), Option[Option[F[B]]]), List[Parameter]] =
//      at[((A, Seq[String]), Option[Option[F[B]]])]{ case ((key, prefix), default) => List(ProductParameter(prefix :+ key.name, required = false, traversableTtToString[B, F], default.flatten.map(_.map(shows.value.show).mkString(",")))) }
  }

//  object folder extends Poly2 {
//    implicit def caseHList[A, B <: HList]: Case.Aux[NewParameter[A], B, NewParameter[A] :: B] =
//      at[NewParameter[A], B]((a, b) => a :: b)
//
//    implicit def caseHLists[A <: HList, B <: HList, Out <: HList](implicit adjoin: Adjoin.Aux[A :: B, Out]): Case.Aux[A, B, Out] =
//      at[A, B]((a, b) => (a :: b).adjoined)
//  }

  def ttToString[T](implicit tag: TypeTag[T]): String = tag.tpe.typeSymbol.name.toString
  def traversableTtToString[T, F[V] <: TraversableOnce[V]](implicit tag: TypeTag[T], traversableTag: TypeTag[F[T]]): String =
    s"${ttToString[F[T]]}[${ttToString[T]}]"
}

case class Parameters[T](eval: Seq[String] => List[Parameter])

object Parameters extends ParametersInstances {
  def apply[T](implicit params: Parameters[T]): Parameters[T] = params
}

case class TypeNames[T](values: List[String])

object TypeNames extends ParametersInstances {
  def apply[T](implicit typeNames: TypeNames[T]): TypeNames[T] = typeNames
}

trait Parameter {
  def path: Seq[String]
  def required: Boolean
  def default: Option[String]
  def updateRequired(req: Boolean): Parameter
  def formatPath(fmt: Seq[String] => String): String = fmt(path)
}

case class ProductParameter(path: Seq[String], required: Boolean, `type`: String, default: Option[String]) extends Parameter {
  override def updateRequired(req: Boolean): ProductParameter = copy(required = req)
}

case class UnionParameter(path: Seq[String], types: NonEmptyList[String]) extends Parameter {
  override val required: Boolean = true
  override val default: Option[String] = None
  override def updateRequired(req: Boolean): UnionParameter = this
}

trait NewParameters[T] extends Serializable {
  type Repr

  def eval(path: Seq[String]): Repr
}

object NewParameters extends ParametersInstances {
  type Aux[T0, Repr0] = NewParameters[T0] { type Repr = Repr0 }

  def apply[T](implicit params: NewParameters[T]): Aux[T, params.Repr] = params
}

trait NewParameter[T] {
  def `type`: String
  def path: Seq[String]
  def required: Boolean
  def default: Option[T]
  def updateRequired(req: Boolean): NewParameter[T]
  def formatPath(fmt: Seq[String] => String): String = fmt(path)
}

case class NewProductParameter[T](path: Seq[String], required: Boolean, `type`: String, default: Option[T]) extends NewParameter[T] {
  override def updateRequired(req: Boolean): NewProductParameter[T] = copy(required = req)
}

case class NewUnionParameter[T](path: Seq[String], `type`: String, types: List[String]) extends NewParameter[T] {
  override val required: Boolean = true
  override val default: Option[T] = None
  override def updateRequired(req: Boolean): NewUnionParameter[T] = this
}