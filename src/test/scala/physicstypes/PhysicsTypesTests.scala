package physicstypes

import physicstypes.Physic.*
import physicstypes.internal.*
import scala.compiletime.testing.{typeCheckErrors, Error}

class PhysicsTypesTests extends munit.FunSuite {

  // The following test does not actually need to run.
  // If it compiles, it's correct.
  test("Type level equality tests") {

    // Checking that Scalar reduction is a Scalar
    summon[ProductReduction[Scalar] =:= Scalar]

    // Checking that Singleton reduction keeps the Singleton
    summon[ProductReduction[Meter **: Scalar] =:= Meter **: Scalar]
    summon[
      ProductReduction[Inverse[Meter] **: Scalar] =:= Inverse[Meter] **: Scalar
    ]

    // Checking that square reduction keeps as is
    summon[
      ProductReduction[Meter **: Meter **: Scalar] =:= Meter **: Meter **: Scalar
    ]

    // Checking that m/m gets simplified to Scalar
    summon[
      ProductReduction[
        Meter **: Inverse[Meter] **: Scalar
      ] =:= Scalar
    ]
    summon[
      ProductReduction[Inverse[Meter] **: Meter **: Scalar] =:= Scalar
    ]

    // Checking that 1/(1/m) gets simplified to m
    summon[InverseOf[Inverse[Meter]] =:= Meter]
    summon[InverseOf[Meter] =:= Inverse[Meter]]

    // Checking that Inverse of a Scalar is a Scalar
    summon[InverseOfPhysicQuantity[Scalar] =:= Scalar]
    summon[
      InverseOfPhysicQuantity[Meter **: Scalar] =:= Inverse[Meter] **: Scalar
    ]

    // Checking that Inverse of product is the product of inverses (in the right order)
    summon[
      InverseOfPhysicQuantity[Meter **: Second **: Scalar] =:=
        Inverse[Meter] **: Inverse[Second] **: Scalar
    ]

  }

  test("One meter should be equal to one meter") {
    assertEquals(1.meter, 1.meter)
  }

  test("Sum is commutative") {
    assertEquals(1.meter + 2.meters, 2.meters + 1.meter)
  }

  test("Sum is associative") {
    assertEquals(
      (1.meters + 2.meters) + 3.meters,
      1.meters + (2.meters + 3.meters)
    )
  }

  test("Product is commutative") {
    assertEquals(1.meters * 2.seconds, 2.seconds * 1.meters)
  }

  test("Product is associative") {
    assertEquals(
      (1.meters * 2.seconds) * 3.kilos,
      1.meters * (2.seconds * 3.kilos)
    )
  }

  test("Doubles and Ints are implicitly converted to Scalar") {
    assertEquals(1.0: Physic[Scalar], 1.0.asScalar)
    assertEquals(1.0.meters * 3, 3.meters)
    assertEquals(3 * oneMeter, 3.meters)
    assertEquals(3.0 * oneMeter, 3.meters)
  }

  test("Power gets correct run-time value") {
    assertEquals(3.meters.power[2], 9.meters * oneMeter)
    assertEquals(3.meters.power[-1], 1 / 3.meters)
    assertEquals(3.meters.power[0], 1.asScalar)
    assertEquals(3.meters.power[-2], 1 / (9.meters * oneMeter))
  }

  test("Can't check equality of double with meter") {
    val compileErrors = typeCheckErrors(
      """
        val eq = 1.0 == 1.meters
      """
    )
    assertOneCompileErrorContains(
      compileErrors,
      "Values of types Double and physicstypes.Physic[physicstypes.Meter **: physicstypes.Scalar]"
    )
  }

  test("Can't sum meters and seconds") {
    val compileErrors = typeCheckErrors(
      """
        val sum = Physic.meters(1.0) + Physic.seconds(2.0)
      """
    )
    assertOneCompileErrorContains(
      compileErrors,
      "Required: physicstypes.Physic[physicstypes.Meter **: physicstypes.Scalar]"
    )
  }

  test("Gravitation force between the earth and the moon") {
    val force: Physic.Force =
      G * earthMass * moonMass / earthMoonDistance.power[2]

    assertEquals(force, 1.982110729079252e20 * oneForce)
  }

  test("Bad gravitation force should not compile") {
    val compileErrors = typeCheckErrors(
      """
        val force: Physic.Force = G * earthMass * moonMass / earthMoonDistance
      """
    )
    assertOneCompileErrorContains(
      compileErrors,
      "Required: physicstypes.Physic.Force"
    )
  }

  val earthMass         = 5.972e24.kilos
  val moonMass          = 7.348e22.kilos
  val earthMoonDistance = 384400e3.meters

  val G = 6.67430e-11 * oneMeter.power[3] / oneKilo / oneSecond.power[2]

  private def checkOneCompileErrorContains(errors: Iterable[Error], substring: String) = {
    errors.exists(_.message.contains(substring))
  }

  private def assertOneCompileErrorContains(errors: Iterable[Error], substring: String) = {
    assert(
      checkOneCompileErrorContains(errors, substring),
      s"Expected a compile error with message containing '$substring', but got: $errors"
    )
  }
}
