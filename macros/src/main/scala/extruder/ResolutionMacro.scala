package extruder

import extruder.core.{ConfigValidation, Resolvers}
import shapeless.{SingletonTypeUtils}

import scala.reflect.macros.whitebox

@macrocompat.bundle
class ResolutionMacro(val c: whitebox.Context) extends SingletonTypeUtils {
  import c.universe._

  def resolve[T : c.WeakTypeTag, R : c.WeakTypeTag]: Tree = {
    val `type` = weakTypeOf[T]
    val typeSymbol = `type`.typeSymbol
    val resolverTypeSymbol = weakTypeOf[R].typeSymbol

    val caseClasses: List[Type] =
      if (typeSymbol.asClass.isSealed) findSealedMembers(typeSymbol)
      else if(typeSymbol.asClass.isCaseClass) findCaseClasses(`type`)
      else c.abort(c.enclosingPosition, "Only case classes or sealed traits are permissible")

    if (caseClasses.nonEmpty) {
      c.info(
        c.enclosingPosition,
        s"Creating implicit resolvers for: '${caseClasses.map(_.typeSymbol.fullName).mkString("', '")}'",
        force = false
      )
    }

    val resolvers: List[Tree] = caseClasses.map(tpe =>
      if ((tpe.typeSymbol.asClass.isCaseClass && !(tpe.typeSymbol.isModuleClass || tpe.typeSymbol.isModule))
          || tpe.typeSymbol.asClass.isSealed) {
        q"""
          implicit lazy val ${TermName(tpe.typeSymbol.asClass.name + "Resolver")}: extruder.core.Resolver[$tpe] =
            extruder.core.Extruder[$tpe](primitiveResolvers).${resolverMethod(tpe.typeSymbol)}
        """
      } else {
        q"""
          implicit lazy val ${TermName(tpe.typeSymbol.asClass.name + "Resolver")}: extruder.core.Resolver[$tpe] =
            extruder.core.Resolver(cats.data.Validated.Valid(${c.parse(tpe.typeSymbol.asClass.fullName)}))
        """
      }
    )

    q"""
      new extruder.AggregateResolver[$typeSymbol, $resolverTypeSymbol] {
        override def ${TermName("apply")}(primitiveResolvers: $resolverTypeSymbol): extruder.core.ConfigValidation[$typeSymbol] = {
          import primitiveResolvers._

          ..$resolvers

          extruder.core.Extruder[$typeSymbol](primitiveResolvers).${resolverMethod(typeSymbol)}.read(Seq.empty, None)
        }
      }
    """
  }

  private def resolverMethod(t: Symbol): TermName = TermName(
    if (t.asClass.isSealed) "unionResolver"
    else "productResolver"
  )

  private def findSealedMembers(symbol: Symbol): List[Type] =
    if (symbol.asClass.isSealed) {
      symbol.asClass.knownDirectSubclasses.toList.filter(x => x.isModuleClass || x.isModule || x.asClass.isCaseClass || x.asClass.isSealed).
        flatMap(x =>
          if (x.asClass.isSealed) x.asType.toType :: findSealedMembers(x)
          else if (x.asClass.isCaseClass) x.asType.toType :: findCaseClasses(x.asType.toType)
          else List(x.asType.toType)
        )
    } else { List.empty }

  private def findCaseClasses(in: Type): List[Type] =
    in.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head.map { field =>
      val fieldType = in.decl(field.asTerm.name).typeSignature

      if (fieldType.typeSymbol.fullName == "scala.Option") {
        fieldType.baseType(typeOf[Option[_]].typeSymbol) match {
          case TypeRef(_, _, targ :: Nil) => targ.typeSymbol
          case NoType => c.abort(c.enclosingPosition, "Options in case classes need a known type parameter.")
        }
      } else { fieldType.typeSymbol }
    }.filter(x => (x.asClass.isCaseClass || x.asClass.isSealed) && !x.fullName.contains("scala.")).
      flatMap(x =>
        if (x.asClass.isSealed) x.asType.toType :: findSealedMembers(x)
        else x.asType.toType :: findCaseClasses(x.asType.toType)
      ).distinct
}

trait AggregateResolver[T, R <: Resolvers] {
  def apply(primitiveResolvers: R): ConfigValidation[T]
}
