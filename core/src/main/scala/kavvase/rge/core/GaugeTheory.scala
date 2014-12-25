package kavvase.rge.core

import kavvase.rge.core.math.Matrix

import scalaz.Monoid

sealed trait GaugeTheory[A, Susy[_], Soft[_]] {

  val susyParams: Susy[A]

  val softParams: Soft[A]

}

trait Vectorable[A[_]] {

  def fromVector[B](v: Vector[B]): A[B]

  def toVector[B](a: A[B]): Vector[B]

}

case class MSSM[A](
  susyParams: MSSMSusyParams[A],
  softParams: MSSMSoftParams[A]
) extends GaugeTheory[A, MSSMSusyParams, MSSMSoftParams]

case class MSSMSusyParams[A](
  g1: A,
  g2: A,
  g3: A,
  yU: Matrix[A],
  yD: Matrix[A],
  yE: Matrix[A],
  mu: A
)

object MSSMSusyParams {

  implicit def MSSMSusyMonoid[A](implicit e: Numeric[A]): Monoid[MSSMSusyParams[A]] = new Monoid[MSSMSusyParams[A]] {

    import kavvase.rge.core.matrix._
    import scala.math.Numeric.Implicits._

    def zero: MSSMSusyParams[A] = {
      MSSMSusyParams(
        g1 = e.zero,
        g2 = e.zero,
        g3 = e.zero,
        yU = Matrix.zero(3, 3),
        yD = Matrix.zero(3, 3),
        yE = Matrix.zero(3, 3),
        mu = e.zero
      )
    }

    def append(f1: MSSMSusyParams[A], f2: => MSSMSusyParams[A]): MSSMSusyParams[A] = {
      MSSMSusyParams(
        g1 = f1.g1 + f2.g1,
        g2 = f1.g2 + f2.g2,
        g3 = f1.g3 + f2.g3,
        yU = f1.yU + f2.yU,
        yD = f1.yD + f2.yD,
        yE = f1.yE + f2.yE,
        mu = f1.mu + f2.mu
      )
    }

  }

  implicit def MSSMSusyVectorable: Vectorable[MSSMSusyParams] = new Vectorable[MSSMSusyParams] {

    def fromVector[B](v: Vector[B]): MSSMSusyParams[B] = {
      MSSMSusyParams(
        g1 = v(0),
        g2 = v(1),
        g3 = v(2),
        yU = Matrix(Vector(
          Vector(v(3), v(4), v(5)),
          Vector(v(6), v(7), v(8)),
          Vector(v(9), v(10), v(11)))),
        yD = Matrix(Vector(
          Vector(v(12), v(13), v(14)),
          Vector(v(15), v(16), v(17)),
          Vector(v(18), v(19), v(20)))),
        yE = Matrix(Vector(
          Vector(v(21), v(22), v(23)),
          Vector(v(24), v(25), v(26)),
          Vector(v(27), v(28), v(29)))),
        mu = v(30)
      )
    }

    def toVector[B](a: MSSMSusyParams[B]): Vector[B] = {
      Vector(
        a.g1, a.g2, a.g3,
        a.yU.rows(0)(0), a.yU.rows(0)(1), a.yU.rows(0)(2),
        a.yU.rows(1)(0), a.yU.rows(1)(1), a.yU.rows(1)(2),
        a.yU.rows(2)(0), a.yU.rows(2)(1), a.yU.rows(2)(2),
        a.yD.rows(0)(0), a.yD.rows(0)(1), a.yD.rows(0)(2),
        a.yD.rows(1)(0), a.yD.rows(1)(1), a.yD.rows(1)(2),
        a.yD.rows(2)(0), a.yD.rows(2)(1), a.yD.rows(2)(2),
        a.yE.rows(0)(0), a.yE.rows(0)(1), a.yE.rows(0)(2),
        a.yE.rows(1)(0), a.yE.rows(1)(1), a.yE.rows(1)(2),
        a.yE.rows(2)(0), a.yE.rows(2)(1), a.yE.rows(2)(2),
        a.mu
      )
    }

  }

}

case class MSSMSoftParams[A](
  m1: A,
  m2: A,
  m3: A,
  aU: Matrix[A],
  aD: Matrix[A],
  aE: Matrix[A],
  b: A,
  mHuSq: A,
  mHdSq: A,
  mQSq: Matrix[A],
  mLSq: Matrix[A],
  mUSq: Matrix[A],
  mDSq: Matrix[A],
  mESq: Matrix[A]
)

object MSSMSoftParams {

  implicit def MSSMSoftMonoid[A](implicit e: Numeric[A]): Monoid[MSSMSoftParams[A]] = new Monoid[MSSMSoftParams[A]] {

    import kavvase.rge.core.matrix._
    import scala.math.Numeric.Implicits._

    def zero: MSSMSoftParams[A] = {
      MSSMSoftParams(
        m1 = e.zero,
        m2 = e.zero,
        m3 = e.zero,
        aU = Matrix.zero(3, 3),
        aD = Matrix.zero(3, 3),
        aE = Matrix.zero(3, 3),
        b = e.zero,
        mHuSq = e.zero,
        mHdSq = e.zero,
        mQSq = Matrix.zero(3, 3),
        mLSq = Matrix.zero(3, 3),
        mUSq = Matrix.zero(3, 3),
        mDSq = Matrix.zero(3, 3),
        mESq = Matrix.zero(3, 3)
      )
    }

    def append(f1: MSSMSoftParams[A], f2: => MSSMSoftParams[A]): MSSMSoftParams[A] = {
      MSSMSoftParams(
        m1 = f1.m1 + f2.m1,
        m2 = f1.m2 + f2.m2,
        m3 = f1.m3 + f2.m3,
        aU = f1.aU + f2.aU,
        aD = f1.aD + f2.aD,
        aE = f1.aE + f2.aE,
        b = f1.b + f2.b,
        mHuSq = f1.mHuSq + f2.mHuSq,
        mHdSq = f1.mHdSq + f2.mHdSq,
        mQSq = f1.mQSq + f2.mQSq,
        mLSq = f1.mLSq + f2.mLSq,
        mUSq = f1.mUSq + f2.mUSq,
        mDSq = f1.mDSq + f2.mDSq,
        mESq = f1.mESq + f2.mESq
      )
    }

  }

  implicit def MSSMSoftVectorable: Vectorable[MSSMSoftParams] = new Vectorable[MSSMSoftParams] {

    def fromVector[B](v: Vector[B]): MSSMSoftParams[B] = {
      MSSMSoftParams(
        m1 = v(0),
        m2 = v(1),
        m3 = v(2),
        aU = Matrix(Vector(
          Vector(v(3), v(4), v(5)),
          Vector(v(6), v(7), v(8)),
          Vector(v(9), v(10), v(11)))),
        aD = Matrix(Vector(
          Vector(v(12), v(13), v(14)),
          Vector(v(15), v(16), v(17)),
          Vector(v(18), v(19), v(20)))),
        aE = Matrix(Vector(
          Vector(v(21), v(22), v(23)),
          Vector(v(24), v(25), v(26)),
          Vector(v(27), v(28), v(29)))),
        b = v(30),
        mHuSq = v(31),
        mHdSq = v(32),
        mQSq = Matrix(Vector(
          Vector(v(33), v(34), v(35)),
          Vector(v(36), v(37), v(38)),
          Vector(v(39), v(40), v(41)))),
        mLSq = Matrix(Vector(
          Vector(v(42), v(43), v(44)),
          Vector(v(45), v(46), v(47)),
          Vector(v(48), v(49), v(50)))),
        mUSq = Matrix(Vector(
          Vector(v(51), v(52), v(53)),
          Vector(v(54), v(55), v(56)),
          Vector(v(57), v(58), v(59)))),
        mDSq = Matrix(Vector(
          Vector(v(60), v(61), v(62)),
          Vector(v(63), v(64), v(65)),
          Vector(v(66), v(67), v(68)))),
        mESq = Matrix(Vector(
          Vector(v(69), v(70), v(71)),
          Vector(v(72), v(73), v(74)),
          Vector(v(75), v(76), v(77))))
      )
    }

    def toVector[B](a: MSSMSoftParams[B]): Vector[B] = {
      Vector(
        a.m1, a.m2, a.m3,
        a.aU.rows(0)(0), a.aU.rows(0)(1), a.aU.rows(0)(2),
        a.aU.rows(1)(0), a.aU.rows(1)(1), a.aU.rows(1)(2),
        a.aU.rows(2)(0), a.aU.rows(2)(1), a.aU.rows(2)(2),
        a.aD.rows(0)(0), a.aD.rows(0)(1), a.aD.rows(0)(2),
        a.aD.rows(1)(0), a.aD.rows(1)(1), a.aD.rows(1)(2),
        a.aD.rows(2)(0), a.aD.rows(2)(1), a.aD.rows(2)(2),
        a.aE.rows(0)(0), a.aE.rows(0)(1), a.aE.rows(0)(2),
        a.aE.rows(1)(0), a.aE.rows(1)(1), a.aE.rows(1)(2),
        a.aE.rows(2)(0), a.aE.rows(2)(1), a.aE.rows(2)(2),
        a.b, a.mHuSq, a.mHdSq,
        a.mQSq.rows(0)(0), a.mQSq.rows(0)(1), a.mQSq.rows(0)(2),
        a.mQSq.rows(1)(0), a.mQSq.rows(1)(1), a.mQSq.rows(1)(2),
        a.mQSq.rows(2)(0), a.mQSq.rows(2)(1), a.mQSq.rows(2)(2),
        a.mLSq.rows(0)(0), a.mLSq.rows(0)(1), a.mLSq.rows(0)(2),
        a.mLSq.rows(1)(0), a.mLSq.rows(1)(1), a.mLSq.rows(1)(2),
        a.mLSq.rows(2)(0), a.mLSq.rows(2)(1), a.mLSq.rows(2)(2),
        a.mUSq.rows(0)(0), a.mUSq.rows(0)(1), a.mUSq.rows(0)(2),
        a.mUSq.rows(1)(0), a.mUSq.rows(1)(1), a.mUSq.rows(1)(2),
        a.mUSq.rows(2)(0), a.mUSq.rows(2)(1), a.mUSq.rows(2)(2),
        a.mDSq.rows(0)(0), a.mDSq.rows(0)(1), a.mDSq.rows(0)(2),
        a.mDSq.rows(1)(0), a.mDSq.rows(1)(1), a.mDSq.rows(1)(2),
        a.mDSq.rows(2)(0), a.mDSq.rows(2)(1), a.mDSq.rows(2)(2),
        a.mESq.rows(0)(0), a.mESq.rows(0)(1), a.mESq.rows(0)(2),
        a.mESq.rows(1)(0), a.mESq.rows(1)(1), a.mESq.rows(1)(2),
        a.mESq.rows(2)(0), a.mESq.rows(2)(1), a.mESq.rows(2)(2)
      )
    }

  }

}

