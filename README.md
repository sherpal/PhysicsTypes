# Physics Types

A small implementation of a type-safe unit calculator.

## Why?

The first thing you do when you finish a computation involving physical quantities is checking that the units match. Well, let the compiler do it for you!

## A Small Example

Let's compute the gravitational force between the earth and the moon!

```scala
import physicstypes.Physic.*

val G = 6.67430e-11 * oneMeter.power[3] / oneKilo / oneSecond.power[2]

val earthMass         = 5.972e24.kilos
val moonMass          = 7.348e22.kilos
val earthMoonDistance = 384400e3.meters

val force: Force = G * earthMass * moonMass / earthMoonDistance.power[2]

// The following does not compile betcause the units don't match! 🎉
// val badForce: Force = G * earthMass * moonMass / earthMoonDistance
```
