package extruder.instances

import extruder.effect.EitherTIOEffectSpec.EitherIO
import extruder.effect.{EitherTIOEffectSpec, ExtruderAsyncSpec, ExtruderMonadErrorSpec, ExtruderSyncSpec}

class EitherTIOExtruderAsyncSpec extends ExtruderAsyncSpec[EitherIO, Throwable] with EitherTIOEffectSpec

class EitherTIOExtruderSyncSpec extends ExtruderSyncSpec[EitherIO, Throwable] with EitherTIOEffectSpec

class EitherTIOExtruderMonadErrorSpec extends ExtruderMonadErrorSpec[EitherIO, Throwable] with EitherTIOEffectSpec
