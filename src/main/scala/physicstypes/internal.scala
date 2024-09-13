package physicstypes

import scala.compiletime.ops.int.*

object internal {
  type Filter[
      P <: PhysicQuantity,
      Predicate <: ([_ <: SIOrInverse] =>> Boolean)
  ] <: PhysicQuantity =
    P match {
      case Scalar => Scalar
      case h **: tail =>
        Predicate[h] match {
          case true  => h **: Filter[tail, Predicate]
          case false => Filter[tail, Predicate]
        }
    }

  // QuickSort algorithm on types
  type SortBy[
      P <: PhysicQuantity,
      F <: [_ <: SIOrInverse] =>> Int
  ] <: PhysicQuantity =
    P match {
      case Scalar => Scalar
      case h **: tail =>
        Concat[
          SortBy[Filter[tail, [t] =>> (F[t] < F[h])], F],
          h **: SortBy[Filter[tail, [t] =>> (F[t] >= F[h])], F]
        ]
    }

  type Concat[A <: PhysicQuantity, B <: PhysicQuantity] <: PhysicQuantity =
    A match {
      case Scalar        => B
      case head **: tail => head **: Concat[tail, B]
    }

  type Contains[T <: SIOrInverse, Tup <: PhysicQuantity] <: Boolean =
    Tup match {
      case Scalar     => false
      case T **: _    => true
      case _ **: tail => Contains[T, tail]
    }

  type RemoveFirst[
      T <: SIOrInverse,
      Tup <: PhysicQuantity
  ] <: PhysicQuantity =
    Tup match {
      case Scalar        => Scalar
      case T **: tail    => tail
      case head **: tail => head **: RemoveFirst[T, tail]
    }

  type InverseOf[T <: SIOrInverse] <: SIOrInverse = T match {
    case Inverse[t] => t
    case T          => Inverse[T]
  }

  type InverseOfPhysicQuantity[T <: PhysicQuantity] <: PhysicQuantity =
    T match {
      case Scalar     => Scalar
      case h **: tail => SortBySIOrdering[InverseOf[h] **: InverseOfPhysicQuantity[tail]]
    }

  type Power[P <: PhysicQuantity, N <: Int] <: PhysicQuantity = N match {
    case 0 => Scalar
    case N =>
      (N < 0) match {
        case true  => InverseOfPhysicQuantity[Power[P, Negate[N]]]
        case false => P *** Power[P, N - 1]
      }
  }

  type SIOrderingFn = [T <: SIOrInverse] =>> SIOrdering[T]

  type SortBySIOrdering[P <: PhysicQuantity] = SortBy[P, SIOrderingFn]

  type ProductReduction[P <: PhysicQuantity] <: PhysicQuantity = P match {
    case Scalar => Scalar
    case h **: tail =>
      Contains[InverseOf[h], tail] match {
        case true  => ProductReduction[RemoveFirst[InverseOf[h], tail]]
        case false => h **: ProductReduction[tail]
      }
  }

  /** Final type for the product of two physic quantities.
    *
    * We
    *   - concatenate the two quantities
    *   - reduce inverses that incurred
    *   - sort according to the SIOrdering
    */
  type ***[Left <: PhysicQuantity, Right <: PhysicQuantity] =
    SortBySIOrdering[ProductReduction[Concat[Left, Right]]]
}
