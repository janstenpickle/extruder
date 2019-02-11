package extruder.core

trait Decode { outer: DataSource =>
  type InputData
  type DecodeData
  type DecodeDefault[A]

  def decode[A]: DecodePartiallyAppliedWithDefaultSettings[DecodeDefault, A, Sett, InputData, DecodeData] =
    decodeF[DecodeDefault, A]

  def decodeF[F[_], A]: DecodePartiallyAppliedWithDefaultSettings[F, A, Sett, InputData, DecodeData] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, Sett, InputData, DecodeData] {
      override def defaultSettings: Sett = outer.defaultSettings
    }
}
