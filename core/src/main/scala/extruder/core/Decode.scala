package extruder.core

trait Decode { outer: DataSource =>
  type InputData
  type DecodeData
  type DecodeDefault[A]
  type DSDecoderF[F[_], A] = Decoder[F, Sett, A, DecodeData]
  type DSDecoder[A] = DSDecoderF[DecodeDefault, A]

  def decode[A]: DecodePartiallyAppliedWithDefaultSettings[DecodeDefault, A, Sett, DecodeData, InputData] =
    decodeF[DecodeDefault, A]

  def decodeF[F[_], A]: DecodePartiallyAppliedWithDefaultSettings[F, A, Sett, DecodeData, InputData] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, Sett, DecodeData, InputData] {
      override def defaultSettings: Sett = outer.defaultSettings
    }
}
