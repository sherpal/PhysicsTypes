package physicstypes

import physicstypes.internal.*

opaque type Physic[T <: PhysicQuantity] = Double

object Physic {
  inline def oneSecond: Physic[Second **: Scalar] = 1.0
  inline def oneMeter: Physic[Meter **: Scalar]   = 1.0
  inline def oneKilo: Physic[Kilo **: Scalar]     = 1.0

  extension (value: Double) {
    inline def withUnit[T <: PhysicQuantity]: Physic[T] = value

    inline def meters: Physic[Meter **: Scalar]   = value
    inline def seconds: Physic[Second **: Scalar] = value
    inline def kilos: Physic[Kilo **: Scalar]     = value
    inline def meter: Physic[Meter **: Scalar]    = value
    inline def second: Physic[Second **: Scalar]  = value
    inline def kilo: Physic[Kilo **: Scalar]      = value
    inline def asScalar: Physic[Scalar]           = value
  }

  type Speed = Physic[Meter **: Inverse[Second] **: Scalar] // m/s
  inline def oneSpeed: Speed = oneMeter / oneSecond

  type Acceleration =
    Physic[Meter **: Inverse[Second] **: Inverse[Second] **: Scalar] // m/s^2
  inline def oneAcceleration: Acceleration = oneMeter / oneSecond.power[2]

  type Force = Physic[
    Meter **: Kilo **: Inverse[Second] **: Inverse[Second] **: Scalar
  ] // kg m/s^2
  inline def oneForce: Force = oneKilo * oneMeter / oneSecond.power[2]

  extension [T <: PhysicQuantity](phys: Physic[T])
    inline def +(other: Physic[T]): Physic[T]                            = phys + other
    inline def -(other: Physic[T]): Physic[T]                            = phys - other
    inline def *[U <: PhysicQuantity](other: Physic[U]): Physic[T *** U] = phys * other
    inline def /[U <: PhysicQuantity](
        other: Physic[U]
    ): Physic[T *** InverseOfPhysicQuantity[U]] = phys / other

    inline def power[N <: Int](using inline nValue: ValueOf[N]): Physic[Power[T, N]] =
      (nValue.value < 0) match {
        case true  => 1.0 / (1 to -nValue.value).map(_ => value).product
        case false => (1 to nValue.value).map(_ => value).product
      }

    inline def value: Double = phys

  given Conversion[Double, Physic[Scalar]] = identity
  given Conversion[Int, Physic[Scalar]]    = _.toDouble
}
