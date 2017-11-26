package extruder.instances

import cats.effect.IO
import extruder.effect.{ExtruderAsyncSpec, ExtruderMonadErrorSpec, ExtruderSyncSpec, IOEffectSpec}

class IOExtruderAsyncSpec extends ExtruderAsyncSpec[IO, Throwable] with IOEffectSpec

class IOExtruderSyncSpec extends ExtruderSyncSpec[IO, Throwable] with IOEffectSpec

class IOExtruderMonadErrorSpec extends ExtruderMonadErrorSpec[IO, Throwable] with IOEffectSpec
