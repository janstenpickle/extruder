package extruder.instances

import extruder.effect.EitherTIOEffectSuite.EitherIO
import extruder.effect.{EitherTIOEffectSuite, ExtruderAsyncSuite, ExtruderMonadErrorSuite, ExtruderSyncSuite}

class EitherTIOExtruderAsyncSuite extends ExtruderAsyncSuite[EitherIO, Throwable] with EitherTIOEffectSuite

class EitherTIOExtruderSyncSuite extends ExtruderSyncSuite[EitherIO, Throwable] with EitherTIOEffectSuite

class EitherTIOExtruderMonadErrorSuite extends ExtruderMonadErrorSuite[EitherIO, Throwable] with EitherTIOEffectSuite
