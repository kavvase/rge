package kavvase.rge.core.math

import org.scalacheck.Arbitrary
import org.specs2.scalaz.Spec

import scalaz.scalacheck.ScalazProperties.{equal, functor}

class MatrixCheck extends Spec {

  implicit val arbitraryMatrix = Arbitrary {
    for {
      w <- Arbitrary.arbitrary[Int]
      x <- Arbitrary.arbitrary[Int]
      y <- Arbitrary.arbitrary[Int]
      z <- Arbitrary.arbitrary[Int]
    } yield Matrix(Vector(Vector(w, x), Vector(y, z)))
  }

  checkAll(equal.laws[Matrix[Int]])
  checkAll(functor.laws[Matrix])

}
