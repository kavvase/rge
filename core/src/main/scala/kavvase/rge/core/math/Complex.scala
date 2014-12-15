package kavvase.rge.core.math

case class Complex[A](real: A, imaginary: A)(implicit e: Numeric[A]) {

  def abs: Double = Complex.abs(real, imaginary)

  def conjugate: Complex[A] = Complex.conjugate(real, imaginary)

}

object Complex {

  import scala.math.Numeric.Implicits._

  private def absSq[A](real: A, imaginary: A)(implicit e: Numeric[A]): A = {
    e.plus(e.times(real, real), e.times(imaginary, imaginary))
  }

  private def abs[A](real: A, imaginary: A)(implicit a: Numeric[A]): Double = {
    math.sqrt(absSq(real, imaginary).toDouble())
  }

  private def conjugate[A](real: A, imaginary: A)(implicit e: Numeric[A]): Complex[A] = {
    Complex(real, - imaginary)
  }

  implicit def ComplexNumeric[A](implicit e: Numeric[A]): Numeric[Complex[A]] = new Numeric[Complex[A]] {

    def plus(x: Complex[A], y: Complex[A]): Complex[A] = {
      Complex(e.plus(x.real, y.real), e.plus(x.imaginary, y.imaginary))
    }

    def minus(x: Complex[A], y: Complex[A]): Complex[A] = {
      Complex(e.minus(x.real, y.real), e.minus(x.imaginary, y.imaginary))
    }

    def times(x: Complex[A], y: Complex[A]): Complex[A] = {
      Complex(
        e.minus(e.times(x.real, y.real), e.times(x.imaginary, y.imaginary)),
        e.plus(e.times(x.real, y.imaginary), e.times(x.imaginary, y.real))
      )
    }

    def negate(x: Complex[A]): Complex[A] = {
      Complex(e.negate(x.real), e.negate(x.imaginary))
    }

    def fromInt(x: Int): Complex[A] = {
      Complex(e.fromInt(x), e.zero)
    }

    def toInt(x: Complex[A]): Int = {
      toDouble(x).toInt
    }

    def toLong(x: Complex[A]): Long = {
      toDouble(x).toLong
    }

    def toFloat(x: Complex[A]): Float = {
      toDouble(x).toFloat
    }

    def toDouble(x: Complex[A]): Double = {
      Complex.abs(x.real, x.imaginary)
    }

    def compare(x: Complex[A], y: Complex[A]): Int = {
      e.compare(Complex.absSq(x.real, x.imaginary), Complex.absSq(y.real, y.imaginary))
    }

  }

}
