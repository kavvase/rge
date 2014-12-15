package kavvase.rge.core.math

import org.specs2.mutable.Specification

class ComplexSpec extends Specification {

  "complex" should {

    import scala.math.Numeric.Implicits._

    "be able to calculate its absolute value" in {
      Complex(3, 4).abs mustEqual 5.0
      Complex(3, 5).abs mustEqual math.sqrt(34)
    }

    "be able to calculate its complex conjugate" in {
      Complex(1, 2).conjugate mustEqual Complex(1, -2)
      Complex(1, -2).conjugate mustEqual Complex(1, 2)
    }

    "support addition" in {
      Complex(1, 2) + Complex(3, 4) mustEqual Complex(4, 6)
      Complex(1, 2) + Complex(-3, -4) mustEqual Complex(-2, -2)
      Complex(-1, -2) + Complex(3, 4) mustEqual Complex(2, 2)
    }

    "support subtraction" in {
      Complex(1, 2) - Complex(3, 4) mustEqual Complex(-2, -2)
      Complex(1, 2) - Complex(-3, -4) mustEqual Complex(4, 6)
      Complex(-1, -2) - Complex(3, 4) mustEqual Complex(-4, -6)
    }

    "support multiplication" in {
      Complex(1, 2) * Complex(3, 4) mustEqual Complex(-5, 10)
      Complex(1, 2) * Complex(-3, -4) mustEqual Complex(5, -10)
      Complex(-1, -2) * Complex(3, 4) mustEqual Complex(5, -10)
    }

    "support negation" in {
      - Complex(1, 2) mustEqual Complex(-1, -2)
      - Complex(-1, 2) mustEqual Complex(1, -2)
      - Complex(1, -2) mustEqual Complex(-1, 2)
      - Complex(-1, -2) mustEqual Complex(1, 2)
    }

    "support conversion to int" in {
      Complex(3, 4).toInt mustEqual 5
      Complex(3, 5).toInt mustEqual math.sqrt(34).toInt
    }

    "support conversion to long" in {
      Complex(3, 4).toLong mustEqual 5L
      Complex(3, 5).toLong mustEqual math.sqrt(34).toLong
    }

    "support conversion to float" in {
      Complex(3, 4).toFloat mustEqual 5.0F
      Complex(3, 5).toFloat mustEqual math.sqrt(34).toFloat
    }

    "support conversion to double" in {
      Complex(3, 4).toDouble mustEqual 5.0
      Complex(3, 5).toDouble mustEqual math.sqrt(34)
    }

  }

}
