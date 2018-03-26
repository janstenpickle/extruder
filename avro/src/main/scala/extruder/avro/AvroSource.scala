package extruder.avro

import cats.Traverse
import cats.instances.option._
import cats.syntax.flatMap._
import extruder.core._
import extruder.effect.ExtruderMonadError
import org.apache.avro.generic.{GenericData, GenericRecord}
import shapeless.Refute

import scala.reflect.runtime.universe._

object AvroDecoders extends DerivedDecoders with Decoders with DecodeTypes {

  //  implicit def stringDecoder[F[_]: Eff]: AvroDecoder[F, String] = mkDecoder(_.toString.pure)

  implicit def fromAvroParser[F[_], T](implicit F: Eff[F], parser: AvroParser[T]): AvroDecoder[F, T] = mkDecoder {
    (path, default, input) =>
      getAny(path, input)(parser.parse.andThen[F[T]](_.fold(F.validationFailure, F.pure)))
        .flatMap(orDefault[F, T](default, path))
  }

  implicit def derp[F[_], T](implicit F: Eff[F], refute: Refute[AvroParser[T]], dec: Dec[F, T]): Dec[F, T] =
    mkDecoder {
      // TODO get the class name
      // TODO get all names under that path
      (path, default, input) =>
        dec.read(path, default, input.get(path.reverse(1)).asInstanceOf[GenericData.Record])
    }

  def orDefault[F[_], T](default: Option[T], path: List[String])(value: Option[T])(implicit F: Eff[F]): F[T] =
    (value, default) match {
      case (Some(v), _) => F.pure(v)
      case (None, Some(d)) => F.pure(d)
      case (None, None) => F.missing(s"Could not find value for '${path.mkString(".")}'") // TODO better path
    }

  def getAny[F[_], T](path: List[String], input: GenericRecord)(
    convert: Any => F[T]
  )(implicit F: Eff[F]): F[Option[T]] =
    F.catchNonFatal(path.reverse.headOption.flatMap(name => Option(input.get(name))))
      .flatMap[Option[T]](x => Traverse[Option].sequence[F, T](x.map(convert)))

  override type DecodeData = GenericRecord
  override type Dec[F[_], T] = AvroDecoder[F, T]
  override type InputData = this.type
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = AvroHints

  override protected def mkDecoder[F[_], T](f: (List[String], Option[T], GenericRecord) => F[T]): AvroDecoder[F, T] =
    new AvroDecoder[F, T] {
      override def read(path: List[String], default: Option[T], input: GenericRecord): F[T] = f(path, default, input)
    }

}

trait AvroHints extends Hints

object AvroHints extends HintsCompanion[AvroHints] {
  override implicit val default: AvroHints = new AvroHints {
    override def pathToString(path: List[String]): String = path.mkString(".")
  }
}

trait AvroDecoder[F[_], T] extends Decoder[F, T, GenericRecord]

case class AvroParser[T](parse: Any => Either[String, T])

object AvroParser {
  implicit val stringParser: AvroParser[String] = AvroParser(v => Right(v.toString))

  def fromOption[T](parse: Any => Option[T])(implicit tag: TypeTag[T]): AvroParser[T] =
    AvroParser(
      value =>
        parse(value).fold[Either[String, T]](
          Left(s"Could not parse type '${tag.tpe.typeSymbol.name.toString}' from value'$value'")
        )(Right(_))
    )
}

object Shit extends App {
  case class Test(string: String)

  import AvroDecoders._

  implicitly[AvroDecoder[Validation, Test]]
}
