package physicstypes

import scala.compiletime.ops.int.*

// SI stands for International System of Units
sealed trait SIOrInverse

sealed trait SI extends SIOrInverse

sealed trait Second extends SI
sealed trait Meter  extends SI
sealed trait Kilo   extends SI
sealed trait Kelvin extends SI
sealed trait Ampere extends SI

sealed trait Inverse[T <: SI] extends SIOrInverse

sealed trait PhysicQuantity

sealed trait Scalar                                           extends PhysicQuantity
sealed trait **:[Head <: SIOrInverse, Tail <: PhysicQuantity] extends PhysicQuantity

// this ordering is completely arbitrary
type SIOrdering[T <: SIOrInverse] <: Int = T match {
  case Meter  => 0
  case Second => 1
  case Kilo   => 2
  case Kelvin => 3
  case Ampere => 4
  // putting inverses at the end
  case Inverse[x] => 1000 + SIOrdering[x]
}
