package kavvase.rge.core.math

import kavvase.rge.core.math.RungeKutta.ODE

trait RungeKutta[A <: ButcherTableau, B] {

  def run(ode: ODE[B], t0: B, y0: Vector[B], h: B): Iterator[(B, Vector[B])] = {
    Iterator.iterate((t0, y0)) { case (t, y) => rungeKuttaStep(ode, t, y, h) }
  }

  def rungeKuttaStep(ode: ODE[B], t: B, y: Vector[B], h: B): (B, Vector[B])

}

object RungeKutta {

  type ODE[A] = (A, Vector[A]) => Vector[A]

  implicit def EulerMethod[A](implicit e: Numeric[A]): RungeKutta[Euler.type, A] = new RungeKutta[Euler.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

  implicit def MidpointMethod[A](implicit e: Numeric[A]): RungeKutta[Midpoint.type, A] = new RungeKutta[Midpoint.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

  implicit def RK4Method[A](implicit e: Numeric[A]): RungeKutta[RK4.type, A] = new RungeKutta[RK4.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

  implicit def FehlbergMethod[A](implicit e: Numeric[A]): RungeKutta[Fehlberg.type, A] = new RungeKutta[Fehlberg.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

  implicit def DormandPrinceMethod[A](implicit e: Numeric[A]): RungeKutta[DormandPrince.type, A] = new RungeKutta[DormandPrince.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

  implicit def CashKarpMethod[A](implicit e: Numeric[A]): RungeKutta[CashKarp.type, A] = new RungeKutta[CashKarp.type, A] {

    def rungeKuttaStep(ode: ODE[A], t: A, y: Vector[A], h: A): (A, Vector[A]) = ???

  }

}

sealed trait ButcherTableau

case object Euler extends ButcherTableau

case object Midpoint extends ButcherTableau

case object RK4 extends ButcherTableau

case object Fehlberg extends ButcherTableau

case object DormandPrince extends ButcherTableau

case object CashKarp extends ButcherTableau
