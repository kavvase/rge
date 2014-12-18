package kavvase.rge.core.math

import org.specs2.mutable.Specification

class MatrixSpec extends Specification {

  "matrix" should {

    import kavvase.rge.core.matrix._

    "support addition with matrix" in {
      val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))
      val matrix2 = Matrix(Vector(Vector(2, 3, 4), Vector(5, 6, 7)))

      matrix1 + matrix2 mustEqual Matrix(Vector(Vector(3, 5, 7), Vector(9, 11, 13)))
    }

    "support subtraction with matrix" in {
      val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))
      val matrix2 = Matrix(Vector(Vector(2, 3, 4), Vector(5, 6, 7)))

      matrix1 - matrix2 mustEqual Matrix(Vector(Vector(-1, -1, -1), Vector(-1, -1, -1)))
    }

    "support negation" in {
      val matrix = Matrix(Vector(Vector(1, 2), Vector(3, 4)))

      - matrix mustEqual Matrix(Vector(Vector(-1, -2), Vector(-3, -4)))
    }

    "support multiplication with matrix" in {
      val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))
      val matrix2 = Matrix(Vector(Vector(2, 3), Vector(4, 5), Vector(6, 7)))

      matrix1 * matrix2 mustEqual Matrix(Vector(Vector(28, 34), Vector(64, 79)))
    }

    "support transposition" in {
      val matrix = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))

      matrix.transpose mustEqual Matrix(Vector(Vector(1, 4), Vector(2, 5), Vector(3, 6)))
    }

    "be created by specifying diagonal elements" in {
      val elements = Vector(1, 2, 3)

      Matrix.diagonal(elements) mustEqual Matrix(Vector(Vector(1, 0, 0), Vector(0, 2, 0), Vector(0, 0, 3)))
    }

  }

}