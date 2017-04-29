package extruder.core

import cats.syntax.validated._

trait Decoders[C, D[T] <: Decoder[T, C]] {
  protected def mkDecoder[T](f: (Seq[String], Option[T], C) => ConfigValidation[T]): D[T]

  def apply[T](implicit decoder: D[T]): D[T] = decoder
}

trait Decode[C, I, D[T] <: Decoder[T, I]] { self: ResolutionCommon =>
  protected def prepareConfig(config: C): ConfigValidation[I]

  def decode[T](config: C)(implicit decoder: D[T]): ConfigValidation[T] =
    decode(Seq.empty, config)

  def decode[T](namespace: Seq[String], config: C)(implicit decoder: D[T]): ConfigValidation[T] =
    prepareConfig(config).fold(_.invalid, c => decoder.read(namespace, None, c))

//  def parameters[T](implicit params: NewParameters[T]): params.Repr =
//    parameters(Seq.empty[String])

  def parameters[T](namespace: Seq[String])(implicit params: NewParameters[T]): params.Repr =
    params.eval(namespace)//.map { case (k, (default, tpe, required)) => ParametersOutput(pathToString(k), default, tpe, required) }.toList
}

trait Decoder[T, C] {
  def read(path: Seq[String], default: Option[T], config: C): ConfigValidation[T]
}

case class ParametersOutput(parameter: String, default: Option[String], `type`: String, required: Boolean)
