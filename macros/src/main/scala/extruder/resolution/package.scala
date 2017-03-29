package extruder

import extruder.core.Resolvers

package object resolution {
  def resolve[T, R <: Resolvers]: AggregateResolver[T, R] = macro ResolutionMacro.resolve[T, R]
}
