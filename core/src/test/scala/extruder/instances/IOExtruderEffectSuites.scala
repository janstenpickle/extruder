package extruder.instances

import cats.effect.IO
import extruder.effect.{ExtruderAsyncSuite, ExtruderMonadErrorSuite, ExtruderSyncSuite, IOEffectSpec}

class IOExtruderAsyncSuite extends ExtruderAsyncSuite[IO, Throwable] with IOEffectSpec

class IOExtruderSyncSuite extends ExtruderSyncSuite[IO, Throwable] with IOEffectSpec

class IOExtruderMonadErrorSuite extends ExtruderMonadErrorSuite[IO, Throwable] with IOEffectSpec
