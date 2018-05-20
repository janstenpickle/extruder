package extruder.metrics.dropwizard

import extruder.core.HintsCompanion
import extruder.metrics.MetricsHints

trait DropwizardHints extends MetricsHints

object DropwizardHints extends HintsCompanion[DropwizardHints] {
  override implicit def default: DropwizardHints = new DropwizardHints {}
}
