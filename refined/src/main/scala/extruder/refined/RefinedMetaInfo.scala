package extruder.refined

import extruder.meta.MetaInfo

trait RefinedMetaInfo[A, F[_, _], P] extends MetaInfo[F[A, P]] {
  def underlying: MetaInfo[A]
}
