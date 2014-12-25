package kavvase.rge.core.math

trait NumericSyntax {

  implicit class NumericOps[A](a: A)(implicit e: Numeric[A]) {

    import scala.math.Numeric.Implicits._

    def power(n: Int): A = {
      loopPower(n)
    }

    private def loopPower(n: Int): A = {
      if (n == 0) e.fromInt(1)
      else if (n % 2 == 0) square(loopPower(n / 2))
      else a * square(loopPower(n / 2))
    }

    private def square(x: A): A = {
      x * x
    }

  }

}
