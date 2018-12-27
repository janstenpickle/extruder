package extruder.refined

import cats.Monad
import eu.timepit.refined.api.{RefType, Validate}
import eu.timepit.refined.shapeless.typeable._
import extruder.core._
import extruder.meta.MetaInfo
import shapeless.Typeable

trait RefinedInstances {
  implicit def refinedDecoder[F[_]: Monad, A, G[_, _], P, S, D](
    implicit decoder: DecoderT[F, S, A, D],
    error: ExtruderErrors[F],
    refType: RefType[G],
    validate: Validate[A, P]
  ): DecoderT[F, S, G[A, P], D] =
    decoder.imapResult(a => error.fromEither(refType.refine[P](a)))(refType.unwrap)

  implicit def refinedEncoder[F[_], A, G[_, _], P, S, D](
    implicit encoder: EncoderT[F, S, A, D],
    refType: RefType[G],
    validate: Validate[A, P]
  ): EncoderT[F, S, G[A, P], D] = encoder.contramap[G[A, P]](refType.unwrap)

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
