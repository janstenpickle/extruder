package extruder.core

trait CombinedDecoderTInstances {
  implicit def combinedDecoder[F[_], A, S0, S1, I0, I1](
    implicit ev0: DecoderT[F, S0, A, I0],
    ev1: DecoderT[F, S1, A, I1],
    errors: ExtruderErrors[F]
  ): DecoderT[F, (S0, S1), A, (I0, I1)] =
    new DecoderT[F, (S0, S1), A, (I0, I1)] {
      override def read(path: List[String], settings: (S0, S1), default: Option[A], input: (I0, I1)): F[A] =
        errors.fallback(ev1.read(path, settings._2, default, input._2))(ev0.read(path, settings._1, default, input._1))
    }
}
