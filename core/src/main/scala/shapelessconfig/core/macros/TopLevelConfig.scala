package shapelessconfig.core.macros

import shapelessconfig.core.Resolvers
import shapelessconfig.core.validation.ConfigValidation

import scala.reflect.macros.whitebox

// TODO make this work with options

object TopLevelConfig {
  def resolve[T <: Product with Serializable, V <: Resolvers]: TopLevelConfig[T] = macro materializeMappableImpl[T, V]

  def materializeMappableImpl[T <: Product with Serializable : c.WeakTypeTag,
                              V <: Resolvers : c.WeakTypeTag](c: whitebox.Context): c.Expr[TopLevelConfig[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val tpeSym = tpe.typeSymbol
    val primitiveResolvers = weakTypeOf[V].typeSymbol

    def findCaseClasses(in: Type): List[Type] =
      in.decls.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.get.paramLists.head.map( field =>
        in.decl(field.asTerm.name).typeSignature.typeSymbol
      ).filter(_.asClass.isCaseClass).
        map(_.asType.toType).
        flatMap(x => x :: findCaseClasses(x))

    val caseClassResolvers = findCaseClasses(tpe).distinct.map(x =>
      q"implicit val ${TermName(x.typeSymbol.asClass.name + "Resolver")}: shapelessconfig.core.Resolver[$x] = shapelessconfig.core.Resolver((_, _) => shapelessconfig.core.ConfigConstructor[$x]().resolve)"
    )

    c.Expr[TopLevelConfig[T]](
      q"""
        new shapelessconfig.core.macros.TopLevelConfig[$tpeSym] {
          val y = new $primitiveResolvers
          import y._

          ..$caseClassResolvers

          override def apply: shapelessconfig.core.validation.ConfigValidation[$tpeSym] = {
            shapelessconfig.core.ConfigConstructor[$tpeSym]().resolve
          }
        }
      """
    )
  }
}

trait TopLevelConfig[T <: Product with Serializable] {
  def apply(): ConfigValidation[T]
}
