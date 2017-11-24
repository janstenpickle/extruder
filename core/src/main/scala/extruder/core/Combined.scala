package extruder.core

import extruder.effect.ExtruderMonadError

class Combined[DD1[F[_], T] <: Decoder[F, T], DD2[F[_], T] <: Decoder[F, T]](
  implicit val d1: DD1[Validation, String],
  val d2: DD2[Validation, String]
) {

  type Dec[F[_], T] = CombinedDecoder.Aux[F, T, DD1, DD2]

  def derp(
    c: (d1.Config, d2.Config)
  )(implicit d: CombinedDecoder.Aux[Validation, String, DD1, DD2]): Validation[String] =
    d.read(List.empty, None, c)

//  override type DecodeData = (d1.Config, d2.Config)
//
//  override type InputData = (d1.Config, d2.Config)
//  override type Eff[F[_]] = ExtruderMonadError[F]
//
//  override protected def prepareInput[F[_]](namespace: List[String], input: (d1.Config, d2.Config))(
//    implicit F: Eff[F],
//    hints: Hint
//  ): F[DecodeData] = F.pure(input)
//
//  override type Hint = MapHints
}

trait CombinedDecoder[F[_], T] extends Decoder[F, T] {
  type D1 <: Decoder[F, T]
  type D2 <: Decoder[F, T]
  val d1: D1
  val d2: D2

  override type Config = (d1.Config, d2.Config)
}

object CombinedDecoder {
  type Aux[F[_], T, DD1[F[_], T] <: Decoder[F, T], DD2[F[_], T] <: Decoder[F, T]] = CombinedDecoder[F, T] {
    type D1 = DD1[F, T]
    type D2 = DD2[F, T]
  }

  implicit def anyDecoder[F[_], T, DD1[F[_], T] <: Decoder[F, T], DD2[F[_], T] <: Decoder[F, T]](
    implicit decoder1: DD1[F, T],
    decoder2: DD2[F, T],
    F: ExtruderMonadError[F]
  ): CombinedDecoder.Aux[F, T, DD1, DD2] =
    new CombinedDecoder[F, T] {
      type D1 = DD1[F, T]
      type D2 = DD2[F, T]

      override val d1: DD1[F, T] = decoder1
      override val d2: DD2[F, T] = decoder2

      override def read(path: List[String], default: Option[T], input: (d1.Config, d2.Config)) =
        F.ap2[T, T, T](F.pure((x, _) => x))(d1.read(path, default, input._1), d2.read(path, default, input._2))

    }
}
