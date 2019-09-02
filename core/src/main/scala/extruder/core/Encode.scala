package extruder.core

trait Encode { outer: DataSource =>
  type EncodeData
  type OutputData
  type EncodeDefault[A]
  type DSEncoderF[F[_], A] = Encoder[F, Sett, A, EncodeData]
  type DSEncoder[A] = DSEncoderF[EncodeDefault, A]

  def encode: EncodePartiallyApplied[EncodeDefault, Sett, EncodeData, OutputData] =
    encodeF[EncodeDefault]

  def encodeF[F[_]]: EncodePartiallyApplied[F, Sett, EncodeData, OutputData] =
    new EncodePartiallyApplied[F, Sett, EncodeData, OutputData] {
      override protected def defaultSettings: Sett = outer.defaultSettings
    }
}
