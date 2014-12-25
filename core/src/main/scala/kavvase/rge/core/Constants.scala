package kavvase.rge.core

trait Constants[A] {

  def pi: A

}

object Constants {

  implicit object DoubleConstants extends Constants[Double] {

    def pi: Double = scala.math.Pi

  }

}
