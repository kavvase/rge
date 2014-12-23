package kavvase.rge.core

import kavvase.rge.core.math.Matrix

trait BetaFunction[A <: GaugeTheory[B], B] {

  def susyBeta(theory: A): SusyBeta

  def softBeta(theory: A): SoftBeta

  def updateSusy(theory: A, params: Vector[B]): A

  def updateSoft(theory: A, params: Vector[B]): A

  trait SusyBeta {
    def toVector: Vector[B]
  }

  trait SoftBeta {
    def toVector: Vector[B]
  }

}

object BetaFunction {

  implicit def MSSMBetaFunction[A]: BetaFunction[MSSM[A], A] = new BetaFunction[MSSM[A], A] {

    def susyBeta(theory: MSSM[A]): SusyBeta = {
      MSSMSusyBeta(
        g1 = ???,
        g2 = ???,
        g3 = ???,
        yU = ???,
        yD = ???,
        yE = ???,
        mu = ???
      )
    }

    def softBeta(theory: MSSM[A]): SoftBeta = {
      MSSMSoftBeta(
        m1 = ???,
        m2 = ???,
        m3 = ???,
        aU = ???,
        aD = ???,
        aE = ???,
        b = ???,
        mHuSq = ???,
        mHdSq = ???,
        mQSq = ???,
        mLSq = ???,
        mUSq = ???,
        mDSq = ???,
        mESq = ???
      )
    }

    def updateSusy(theory: MSSM[A], params: Vector[A]): MSSM[A] = {
      MSSM(
        g1 = params(0),
        g2 = params(1),
        g3 = params(2),
        yU = Matrix(Vector(
          Vector(params(3), params(4), params(5)),
          Vector(params(6), params(7), params(8)),
          Vector(params(9), params(10), params(11)))),
        yD = Matrix(Vector(
          Vector(params(12), params(13), params(14)),
          Vector(params(15), params(16), params(17)),
          Vector(params(18), params(19), params(20)))),
        yE = Matrix(Vector(
          Vector(params(21), params(22), params(23)),
          Vector(params(24), params(25), params(26)),
          Vector(params(27), params(28), params(29)))),
        mu = params(30),
        m1 = theory.m1,
        m2 = theory.m2,
        m3 = theory.m3,
        aU = theory.aU,
        aD = theory.aD,
        aE = theory.aE,
        b = theory.b,
        mHuSq = theory.mHuSq,
        mHdSq = theory.mHdSq,
        mQSq = theory.mQSq,
        mLSq = theory.mLSq,
        mUSq = theory.mUSq,
        mDSq = theory.mDSq,
        mESq = theory.mESq
      )
    }

    def updateSoft(theory: MSSM[A], params: Vector[A]): MSSM[A] = {
      MSSM(
        g1 = theory.g1,
        g2 = theory.g2,
        g3 = theory.g3,
        yU = theory.yU,
        yD = theory.yD,
        yE = theory.yE,
        mu = theory.mu,
        m1 = params(0),
        m2 = params(1),
        m3 = params(2),
        aU = Matrix(Vector(
          Vector(params(3), params(4), params(5)),
          Vector(params(6), params(7), params(8)),
          Vector(params(9), params(10), params(11)))),
        aD = Matrix(Vector(
          Vector(params(12), params(13), params(14)),
          Vector(params(15), params(16), params(17)),
          Vector(params(18), params(19), params(20)))),
        aE = Matrix(Vector(
          Vector(params(21), params(22), params(23)),
          Vector(params(24), params(25), params(26)),
          Vector(params(27), params(28), params(29)))),
        b = params(30),
        mHuSq = params(31),
        mHdSq = params(32),
        mQSq = Matrix(Vector(
          Vector(params(33), params(34), params(35)),
          Vector(params(36), params(37), params(38)),
          Vector(params(39), params(40), params(41)))),
        mLSq = Matrix(Vector(
          Vector(params(42), params(43), params(44)),
          Vector(params(45), params(46), params(47)),
          Vector(params(48), params(49), params(50)))),
        mUSq = Matrix(Vector(
          Vector(params(51), params(52), params(53)),
          Vector(params(54), params(55), params(56)),
          Vector(params(57), params(58), params(59)))),
        mDSq = Matrix(Vector(
          Vector(params(60), params(61), params(62)),
          Vector(params(63), params(64), params(65)),
          Vector(params(66), params(67), params(68)))),
        mESq = Matrix(Vector(
          Vector(params(69), params(70), params(71)),
          Vector(params(72), params(73), params(74)),
          Vector(params(75), params(76), params(77))))
      )
    }

    case class MSSMSusyBeta(
      g1: A,
      g2: A,
      g3: A,
      yU: Matrix[A],
      yD: Matrix[A],
      yE: Matrix[A],
      mu: A
    ) extends SusyBeta {

      def toVector: Vector[A] = {
        Vector(
          g1, g2, g3,
          yU.rows(0)(0), yU.rows(0)(1), yU.rows(0)(2),
          yU.rows(1)(0), yU.rows(1)(1), yU.rows(1)(2),
          yU.rows(2)(0), yU.rows(2)(1), yU.rows(2)(2),
          yD.rows(0)(0), yD.rows(0)(1), yU.rows(0)(2),
          yD.rows(1)(0), yD.rows(1)(1), yU.rows(1)(2),
          yD.rows(2)(0), yD.rows(2)(1), yU.rows(2)(2),
          yE.rows(0)(0), yE.rows(0)(1), yE.rows(0)(2),
          yE.rows(1)(0), yE.rows(1)(1), yE.rows(1)(2),
          yE.rows(2)(0), yE.rows(2)(1), yE.rows(2)(2),
          mu
        )
      }

    }

    case class MSSMSoftBeta(
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
    ) extends SoftBeta {

      def toVector: Vector[A] = {
        Vector(
          m1, m2, m3,
          aU.rows(0)(0), aU.rows(0)(1), aU.rows(0)(2),
          aU.rows(1)(0), aU.rows(1)(1), aU.rows(1)(2),
          aU.rows(2)(0), aU.rows(2)(1), aU.rows(2)(2),
          aD.rows(0)(0), aD.rows(0)(1), aD.rows(0)(2),
          aD.rows(1)(0), aD.rows(1)(1), aD.rows(1)(2),
          aD.rows(2)(0), aD.rows(2)(1), aD.rows(2)(2),
          aE.rows(0)(0), aE.rows(0)(1), aE.rows(0)(2),
          aE.rows(1)(0), aE.rows(1)(1), aE.rows(1)(2),
          aE.rows(2)(0), aE.rows(2)(1), aE.rows(2)(2),
          b, mHuSq, mHdSq,
          mQSq.rows(0)(0), mQSq.rows(0)(1), mQSq.rows(0)(2),
          mQSq.rows(1)(0), mQSq.rows(1)(1), mQSq.rows(1)(2),
          mQSq.rows(2)(0), mQSq.rows(2)(1), mQSq.rows(2)(2),
          mLSq.rows(0)(0), mLSq.rows(0)(1), mLSq.rows(0)(2),
          mLSq.rows(1)(0), mLSq.rows(1)(1), mLSq.rows(1)(2),
          mLSq.rows(2)(0), mLSq.rows(2)(1), mLSq.rows(2)(2),
          mUSq.rows(0)(0), mUSq.rows(0)(1), mUSq.rows(0)(2),
          mUSq.rows(1)(0), mUSq.rows(1)(1), mUSq.rows(1)(2),
          mUSq.rows(2)(0), mUSq.rows(2)(1), mUSq.rows(2)(2),
          mDSq.rows(0)(0), mDSq.rows(0)(1), mDSq.rows(0)(2),
          mDSq.rows(1)(0), mDSq.rows(1)(1), mDSq.rows(1)(2),
          mDSq.rows(2)(0), mDSq.rows(2)(1), mDSq.rows(2)(2),
          mESq.rows(0)(0), mESq.rows(0)(1), mESq.rows(0)(2),
          mESq.rows(1)(0), mESq.rows(1)(1), mESq.rows(1)(2),
          mESq.rows(2)(0), mESq.rows(2)(1), mESq.rows(2)(2)
        )
      }

    }

  }

}
