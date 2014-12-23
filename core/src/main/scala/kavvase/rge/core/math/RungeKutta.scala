package kavvase.rge.core.math

import kavvase.rge.core.math.RungeKutta.Function

import scalaz.Zip
import scalaz.Scalaz._

trait RungeKutta[A <: ButcherTableau, B] {

  def run(f: Function[B], t0: B, ys0: Vector[B], h: B): Iterator[(B, Vector[B])] = {
    Iterator.iterate((t0, ys0)) { case (t, ys) => rungeKuttaStep(f, t, ys, h) }
  }

  def rungeKuttaStep(f: Function[B], t: B, ys: Vector[B], h: B): (B, Vector[B])

}

object RungeKutta {

  import scala.math.Fractional.Implicits._

  type Function[A] = (A, Vector[A]) => Vector[A]

  implicit def EulerMethod[A](implicit e: Fractional[A]): RungeKutta[Euler.type, A] = {
    new RungeKutta[Euler.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks = f(t, ys)
        (t + h, (ys, ks).zipped.map(_ + _ * h))
      }

    }
  }

  implicit def MidpointMethod[A](implicit e: Fractional[A]): RungeKutta[Midpoint.type, A] = {
    new RungeKutta[Midpoint.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks1 = f(t, ys)
        val ks2 = f(t + h / e.fromInt(2), (ys, ks1).zipped.map(_ + _ * h / e.fromInt(2)))
        (t + h, (ys, ks2).zipped.map(_ + _ * h))
      }

    }
  }

  implicit def RK4Method[A](implicit e: Fractional[A]): RungeKutta[RK4.type, A] = {
    new RungeKutta[RK4.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks1 = f(t, ys)
        val ks2 = f(t + h / e.fromInt(2), (ys, ks1).zipped.map(_ + _ * h / e.fromInt(2)))
        val ks3 = f(t + h / e.fromInt(2), (ys, ks2).zipped.map(_ + _ * h / e.fromInt(2)))
        val ks4 = f(t + h, (ys, ks3).zipped.map(_ + _ * h))
        (t + h, Zip[Vector].ap.tuple5(ys, ks1, ks2, ks3, ks4).map { case (y, k1, k2, k3, k4) =>
          y + (k1 + e.fromInt(2) * k2 + e.fromInt(2) * k3 + k4) * h / e.fromInt(6)
        })
      }

    }
  }

  implicit def CashKarpMethod[A](implicit e: Fractional[A]): RungeKutta[CashKarp.type, A] = {
    new RungeKutta[CashKarp.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks1 = f(t, ys)
        val ks2 = f(t + h / e.fromInt(5), (ys, ks1).zipped.map(_ * _ * h / e.fromInt(5)))
        val ks3 = f(
          t + h * e.fromInt(3) / e.fromInt(10),
          Zip[Vector].ap.tuple3(ys, ks1, ks2).map { case (y, k1, k2) =>
            y + h * (k1 * e.fromInt(3) + k2 * e.fromInt(9)) / e.fromInt(40)
          })
        val ks4 = f(
          t + h * e.fromInt(3) / e.fromInt(5),
          Zip[Vector].ap.tuple4(ys, ks1, ks2, ks3).map { case (y, k1, k2, k3) =>
            y + h * (k1 * e.fromInt(3) / e.fromInt(10) + k2 * e.fromInt(-9) / e.fromInt(10) +
              k3 * e.fromInt(6) / e.fromInt(5))
          })
        val ks5 = f(
          t + h,
          Zip[Vector].ap.tuple5(ys, ks1, ks2, ks3, ks4).map { case (y, k1, k2, k3, k4) =>
            y + h * (k1 * e.fromInt(-11) / e.fromInt(54) + k2 * e.fromInt(5) / e.fromInt(2) +
              k3 * e.fromInt(-70) / e.fromInt(27) + k4 * e.fromInt(35) / e.fromInt(27))
          })
        val ks6 = f(
          t + h * e.fromInt(7) / e.fromInt(8),
          Zip[Vector].ap.apply6(ys, ks1, ks2, ks3, ks4, ks5)((_, _, _, _, _, _)).map {
            case (y, k1, k2, k3, k4, k5) =>
              y + h * (k1 * e.fromInt(1631) / e.fromInt(55296) + k2 * e.fromInt(175) / e.fromInt(512) +
                k3 * e.fromInt(575) / e.fromInt(13824) + k4 * e.fromInt(44275) / e.fromInt(110592) +
                k5 * e.fromInt(253) / e.fromInt(4096))
          })
        (t + h, Zip[Vector].ap.tuple5(ys, ks1, ks3, ks4, ks6).map { case (y, k1, k3, k4, k6) =>
          y + (k1 * e.fromInt(37) / e.fromInt(378) + k3 * e.fromInt(250) / e.fromInt(621) +
            k4 * e.fromInt(125) / e.fromInt(594) + k6 * e.fromInt(512) / e.fromInt(1771))
        })
      }

    }
  }


  implicit def FehlbergMethod[A](implicit e: Fractional[A]): RungeKutta[Fehlberg.type, A] = {
    new RungeKutta[Fehlberg.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks1 = f(t, ys)
        val ks2 = f(t + h / e.fromInt(4), (ys, ks1).zipped.map(_ + _ * h / e.fromInt(4)))
        val ks3 = f(
          t + h * e.fromInt(3) / e.fromInt(8),
          Zip[Vector].ap.tuple3(ys, ks1, ks2).map { case (y, k1, k2) =>
            y + h * (k1 * e.fromInt(3) + k2 * e.fromInt(9)) / e.fromInt(32)
          })
        val ks4 = f(
          t + h * e.fromInt(12) / e.fromInt(13),
          Zip[Vector].ap.tuple4(ys, ks1, ks2, ks3).map { case (y, k1, k2, k3) =>
            y + h * (k1 * e.fromInt(1932) + k2 * e.fromInt(-7200) + k3 * e.fromInt(7296)) / e.fromInt(2197)
          })
        val ks5 = f(
          t + h,
          Zip[Vector].ap.tuple5(ys, ks1, ks2, ks3, ks4).map { case (y, k1, k2, k3, k4) =>
            y + h * (k1 * e.fromInt(439) / e.fromInt(216) + k2 * e.fromInt(-8) +
              k3 * e.fromInt(3680) / e.fromInt(513) + k4 * e.fromInt(-845) / e.fromInt(4104))
          })
        val ks6 = f(
          t + h / e.fromInt(2),
          Zip[Vector].ap.apply6(ys, ks1, ks2, ks3, ks4, ks5)((_, _, _, _, _, _)).map {
            case (y, k1, k2, k3, k4, k5) =>
              y + h * (k1 * e.fromInt(-8) / e.fromInt(27) + k2 * e.fromInt(2) +
                k3 * e.fromInt(-3544) / e.fromInt(2565) + k4 * e.fromInt(1859) / e.fromInt(4104) +
                k5 * e.fromInt(-11) / e.fromInt(40))
          }
        )
        (t + h, Zip[Vector].ap.apply6(ys, ks1, ks3, ks4, ks5, ks6)((_, _, _, _, _, _)).map {
          case (y, k1, k3, k4, k5, k6) =>
            y + (k1 * e.fromInt(16) / e.fromInt(135) + k3 * e.fromInt(6656) / e.fromInt(12825) +
              k4 * e.fromInt(28561) / e.fromInt(56430) + k5 * e.fromInt(-9) / e.fromInt(50) +
              k6 * e.fromInt(2) / e.fromInt(55)) * h
        })
      }

    }
  }

  implicit def DormandPrinceMethod[A](implicit e: Fractional[A]): RungeKutta[DormandPrince.type, A] = {
    new RungeKutta[DormandPrince.type, A] {

      def rungeKuttaStep(f: Function[A], t: A, ys: Vector[A], h: A): (A, Vector[A]) = {
        val ks1 = f(t, ys)
        val ks2 = f(t + h / e.fromInt(5), (ys, ks1).zipped.map(_ + _ * h / e.fromInt(5)))
        val ks3 = f(
          t + h * e.fromInt(3) / e.fromInt(10),
          Zip[Vector].ap.tuple3(ys, ks1, ks2).map { case (y, k1, k2) =>
            y + h * (k1 * e.fromInt(3) + k2 * e.fromInt(9)) / e.fromInt(40)
          })
        val ks4 = f(
          t + h * e.fromInt(4) / e.fromInt(5),
          Zip[Vector].ap.tuple4(ys, ks1, ks2, ks3).map { case (y, k1, k2, k3) =>
            y + h * (k1 * e.fromInt(44) / e.fromInt(45) + k2 * e.fromInt(-56) / e.fromInt(15) +
              k3 * e.fromInt(32) / e.fromInt(9))
          })
        val ks5 = f(
          t + h * e.fromInt(8) / e.fromInt(9),
          Zip[Vector].ap.tuple5(ys, ks1, ks2, ks3, ks4).map { case (y, k1, k2, k3, k4) =>
            y + h * (k1 * e.fromInt(19372) / e.fromInt(6561) + k2 * e.fromInt(-25360) / e.fromInt(2187) +
              k3 * e.fromInt(64448) / e.fromInt(6561) + k4 * e.fromInt(-212) / e.fromInt(729))
          })
        val ks6 = f(
          t + h,
          Zip[Vector].ap.apply6(ys, ks1, ks2, ks3, ks4, ks5)((_, _, _, _, _, _)).map {
            case (y, k1, k2, k3, k4, k5) =>
              y + h * (k1 * e.fromInt(9017) / e.fromInt(3168) + k2 * e.fromInt(-355) / e.fromInt(33) +
                k3 * e.fromInt(46732) / e.fromInt(5247) + k4 * e.fromInt(49) / e.fromInt(176) +
                k5 * e.fromInt(-5103) / e.fromInt(18656))
          }
        )
        (t + h, Zip[Vector].ap.apply6(ys, ks1, ks3, ks4, ks5, ks6)((_, _, _, _, _, _)).map {
          case (y, k1, k3, k4, k5, k6) =>
            y + (k1 * e.fromInt(35) / e.fromInt(384) + k3 * e.fromInt(500) / e.fromInt(1113) +
              k4 * e.fromInt(125) / e.fromInt(192) + k5 * e.fromInt(-2187) / e.fromInt(6784) +
              k6 * e.fromInt(11) / e.fromInt(84)) * h
        })
      }

    }
  }

}

sealed trait ButcherTableau

case object Euler extends ButcherTableau

case object Midpoint extends ButcherTableau

case object RK4 extends ButcherTableau

case object CashKarp extends ButcherTableau

case object Fehlberg extends ButcherTableau

case object DormandPrince extends ButcherTableau
