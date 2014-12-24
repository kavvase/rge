package kavvase.rge.core.math

case class Matrix[A](rows: Vector[Vector[A]]) {

  def transpose: Matrix[A] = Matrix(rows.transpose)

}

object Matrix {

  def diagonal[A](elements: Vector[A])(implicit e: Numeric[A]): Matrix[A] = {
    val dim = elements.length
    val rows = Vector.range(0, dim).map { i =>
      elements.zipWithIndex.map { case (elem, j) => if (j == i) elem else e.zero}
    }
    Matrix(rows)
  }

  def zero[A](numRow: Int, numCol: Int)(implicit e: Numeric[A]): Matrix[A] = {
    Matrix(Vector.fill(numRow, numCol)(e.zero))
  }

}

trait MatrixSyntax {

  implicit class MatrixOps[A](lhs: Matrix[A])(implicit e: Numeric[A]) {

    import scala.math.Numeric.Implicits._

    def +(rhs: Matrix[A]): Matrix[A] = {
      Matrix((lhs.rows, rhs.rows).zipped.map((l, r) => (l, r).zipped.map(_ + _)))
    }

    def -(rhs: Matrix[A]): Matrix[A] = - rhs + lhs

    def *(rhs: Matrix[A]): Matrix[A] = {
      Matrix(lhs.rows.map(row => rhs.transpose.rows.map(col => (col, row).zipped.map(_ * _).sum)))
    }

    def unary_-(): Matrix[A] = Matrix(lhs.rows.map(_.map(-_)))

  }

}