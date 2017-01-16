package extruder

import macrocompat.bundle
import extruder.core.Resolvers
import extruder.syntax.validation.ConfigValidation

import scala.reflect.macros.whitebox

object resolution {
  def resolve[T <: Product with Serializable]: AggregateResolver[T] = macro ResolutionMacro.resolve[T]
}

@bundle
class ResolutionMacro(val c: whitebox.Context) {
  import c.universe._

  def resolve[T <: Product with Serializable : c.WeakTypeTag]: Tree = {
    val `type` = weakTypeOf[T]
    val typeSymbol = `type`.typeSymbol

    def findCaseClasses(in: Type): List[Type] =
      in.decls.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.get.paramLists.head.map { field =>
        val fieldType = in.decl(field.asTerm.name).typeSignature

        if (fieldType.typeSymbol.fullName == "scala.Option")
          fieldType.baseType(typeOf[Option[_]].typeSymbol) match {
            case TypeRef(_, _, targ :: Nil) => targ.typeSymbol
            case NoType => c.abort(c.enclosingPosition, "Options in case classes need a known type parameter.")
          }
        else fieldType.typeSymbol
      }.filter(_.asClass.isCaseClass).
        map(_.asType.toType).
        flatMap(x => x :: findCaseClasses(x)).distinct

    val caseClasses = findCaseClasses(`type`)

    c.info(
      c.enclosingPosition,
      s"Creating implicit resolvers for case classes: '${caseClasses.map(_.typeSymbol.fullName).mkString("', '")}'",
      force = false
    )

    def caseClassResolvers(includePrefix: Boolean): List[Tree] = caseClasses.map(cc =>
      q"""
        implicit lazy val ${TermName(cc.typeSymbol.asClass.name + "Resolver")}: extruder.core.Resolver[$cc] =
          extruder.core.Resolver[$cc]((path, _) =>
            extruder.core.Extruder[$cc](${if (includePrefix) q"Some(path)" else q"None"}).resolve
          )
      """
    )

    def method(name: String, includePrefix: Boolean): Tree = q"""
      override def ${TermName(name)}(primitiveResolvers: extruder.core.Resolvers): extruder.syntax.validation.ConfigValidation[$typeSymbol] = {
        import primitiveResolvers._

        ..${caseClassResolvers(includePrefix)}

        extruder.core.Extruder[$typeSymbol]().resolve
      }
    """

    q"""
      new extruder.AggregateResolver[$typeSymbol] {
        ${method("apply", includePrefix = true)}
        ${method("singletons", includePrefix = false)}
      }
    """
  }
}

trait AggregateResolver[T <: Product with Serializable] {
  def apply(primitiveResolvers: Resolvers): ConfigValidation[T]
  def singletons(primitiveResolvers: Resolvers): ConfigValidation[T]
}
