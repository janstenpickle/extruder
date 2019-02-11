package extruder.core

trait Encode { outer: DataSource =>
  type EncodeData
  type OutputData
  type EncodeDefault[A]

  def encode: EncodePartiallyAppliedWithDefaultSettings[EncodeDefault, Sett, EncodeData, OutputData] =
    encodeF[EncodeDefault]

  def encodeF[F[_]]: EncodePartiallyAppliedWithDefaultSettings[F, Sett, EncodeData, OutputData] =
    new EncodePartiallyAppliedWithDefaultSettings[F, Sett, EncodeData, OutputData] {
      override protected def defaultSettings: Sett = outer.defaultSettings
    }
}
