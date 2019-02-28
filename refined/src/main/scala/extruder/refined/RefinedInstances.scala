package extruder.refined

import cats.Monad
import eu.timepit.refined.api.{RefType, Validate}
import eu.timepit.refined.shapeless.typeable._
import extruder.core._
import extruder.meta.MetaInfo
import shapeless.Typeable

trait RefinedInstances {
  implicit def refinedDecoder[F[_]: Monad, A, G[_, _], P, S, D](
    implicit decoder: Decoder[F, S, A, D],
    error: ExtruderErrors[F],
    refType: RefType[G],
    validate: Validate[A, P]
  ): Decoder[F, S, G[A, P], D] =
    decoder.imapResult(a => error.fromEither(refType.refine[P](a)))(refType.unwrap)

  implicit def refinedParser[A, F[_, _], P](
    implicit parser: Parser[A],
    refType: RefType[F],
    validate: Validate[A, P]
  ): Parser[F[A, P]] =
    parser.flatMapResult(refType.refine[P](_))

  implicit def refinedEncoder[F[_], A, G[_, _], P, S, D](
    implicit encoder: Encoder[F, S, A, D],
    refType: RefType[G],
    validate: Validate[A, P]
  ): Encoder[F, S, G[A, P], D] = encoder.contramap[G[A, P]](refType.unwrap)

  implicit def refinedShow[A, F[_, _], P](implicit show: Show[A], refType: RefType[F]): Show[F[A, P]] =
    show.contramap(refType.unwrap)

  implicit def refinedMetaInfo[A, F[_, _], P](
    implicit metaInfo: MetaInfo[A],
    rt: RefType[F],
    A: Typeable[A],
    P: Typeable[P],
    V: Validate[A, P]
  ): RefinedMetaInfo[A, F, P] = new RefinedMetaInfo[A, F, P] {
    override val underlying: MetaInfo[A] = metaInfo
    override val typeable: Typeable[F[A, P]] = Typeable[F[A, P]]
    override val `type`: String = typeable.describe
  }
}
