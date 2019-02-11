package extruder.metrics

package object spectator extends SpectatorEncoderInstances with SpectatorEncoder with SpectatorDataSource {
  object encoder extends SpectatorEncoder with SpectatorDataSource
  object instances extends SpectatorEncoderInstances
}
