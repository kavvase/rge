package kavvase.rge.core

import kavvase.rge.core.math.Matrix
import org.specs2.mutable.Specification

class GaugeTheorySpec extends Specification {

  "mssm susy params" should {

    val g1 = 1.0
    val g2 = 2.0
    val g3 = 3.0
    val yU00 = 0.1
    val yU01 = 0.2
    val yU02 = 0.3
    val yU10 = 0.4
    val yU11 = 0.5
    val yU12 = 0.6
    val yU20 = 0.7
    val yU21 = 0.8
    val yU22 = 0.9
    val yD00 = 1.1
    val yD01 = 1.2
    val yD02 = 1.3
    val yD10 = 1.4
    val yD11 = 1.5
    val yD12 = 1.6
    val yD20 = 1.7
    val yD21 = 1.8
    val yD22 = 1.9
    val yE00 = 2.1
    val yE01 = 2.2
    val yE02 = 2.3
    val yE10 = 2.4
    val yE11 = 2.5
    val yE12 = 2.6
    val yE20 = 2.7
    val yE21 = 2.8
    val yE22 = 2.9
    val mu = 4.0

    val params = MSSMSusyParams(
      g1 = g1,
      g2 = g2,
      g3 = g3,
      yU = Matrix(Vector(
        Vector(yU00, yU01, yU02), Vector(yU10, yU11, yU12), Vector(yU20, yU21, yU22))),
      yD = Matrix(Vector(
        Vector(yD00, yD01, yD02), Vector(yD10, yD11, yD12), Vector(yD20, yD21, yD22))),
      yE = Matrix(Vector(
        Vector(yE00, yE01, yE02), Vector(yE10, yE11, yE12), Vector(yE20, yE21, yE22))),
      mu = mu
    )

    val vector = Vector(
      g1, g2, g3,
      yU00, yU01, yU02, yU10, yU11, yU12, yU20, yU21, yU22,
      yD00, yD01, yD02, yD10, yD11, yD12, yD20, yD21, yD22,
      yE00, yE01, yE02, yE10, yE11, yE12, yE20, yE21, yE22,
      mu)

    val vectorable = implicitly[Vectorable[MSSMSusyParams]]

    "be generated from vector" in {
      vectorable.fromVector(vector) mustEqual params
    }

    "be converted to vector" in {
      vectorable.toVector(params) mustEqual vector
    }

  }

  "mssm soft params" should {

    val m1 = 1.0
    val m2 = 2.0
    val m3 = 3.0
    val aU00 = 0.1
    val aU01 = 0.2
    val aU02 = 0.3
    val aU10 = 0.4
    val aU11 = 0.5
    val aU12 = 0.6
    val aU20 = 0.7
    val aU21 = 0.8
    val aU22 = 0.9
    val aD00 = 1.1
    val aD01 = 1.2
    val aD02 = 1.3
    val aD10 = 1.4
    val aD11 = 1.5
    val aD12 = 1.6
    val aD20 = 1.7
    val aD21 = 1.8
    val aD22 = 1.9
    val aE00 = 2.1
    val aE01 = 2.2
    val aE02 = 2.3
    val aE10 = 2.4
    val aE11 = 2.5
    val aE12 = 2.6
    val aE20 = 2.7
    val aE21 = 2.8
    val aE22 = 2.9
    val b = 4.0
    val mHuSq = 5.0
    val mHdSq = 6.0
    val mQSq00 = 3.1
    val mQSq01 = 3.2
    val mQSq02 = 3.3
    val mQSq10 = 3.4
    val mQSq11 = 3.5
    val mQSq12 = 3.6
    val mQSq20 = 3.7
    val mQSq21 = 3.8
    val mQSq22 = 3.9
    val mLSq00 = 4.1
    val mLSq01 = 4.2
    val mLSq02 = 4.3
    val mLSq10 = 4.4
    val mLSq11 = 4.5
    val mLSq12 = 4.6
    val mLSq20 = 4.7
    val mLSq21 = 4.8
    val mLSq22 = 4.9
    val mUSq00 = 5.1
    val mUSq01 = 5.2
    val mUSq02 = 5.3
    val mUSq10 = 5.4
    val mUSq11 = 5.5
    val mUSq12 = 5.6
    val mUSq20 = 5.7
    val mUSq21 = 5.8
    val mUSq22 = 5.9
    val mDSq00 = 6.1
    val mDSq01 = 6.2
    val mDSq02 = 6.3
    val mDSq10 = 6.4
    val mDSq11 = 6.5
    val mDSq12 = 6.6
    val mDSq20 = 6.7
    val mDSq21 = 6.8
    val mDSq22 = 6.9
    val mESq00 = 7.1
    val mESq01 = 7.2
    val mESq02 = 7.3
    val mESq10 = 7.4
    val mESq11 = 7.5
    val mESq12 = 7.6
    val mESq20 = 7.7
    val mESq21 = 7.8
    val mESq22 = 7.9

    val params = MSSMSoftParams(
      m1 = m1,
      m2 = m2,
      m3 = m3,
      aU = Matrix(Vector(
        Vector(aU00, aU01, aU02), Vector(aU10, aU11, aU12), Vector(aU20, aU21, aU22))),
      aD = Matrix(Vector(
        Vector(aD00, aD01, aD02), Vector(aD10, aD11, aD12), Vector(aD20, aD21, aD22))),
      aE = Matrix(Vector(
        Vector(aE00, aE01, aE02), Vector(aE10, aE11, aE12), Vector(aE20, aE21, aE00))),
      b = b,
      mHuSq = mHuSq,
      mHdSq = mHdSq,
      mQSq = Matrix(Vector(
        Vector(mQSq00, mQSq01, mQSq02), Vector(mQSq10, mQSq11, mQSq12), Vector(mQSq20, mQSq21, mQSq22))),
      mLSq = Matrix(Vector(
        Vector(mLSq00, mLSq01, mLSq02), Vector(mLSq10, mLSq11, mLSq12), Vector(mLSq20, mLSq21, mLSq22))),
      mUSq = Matrix(Vector(
        Vector(mUSq00, mUSq01, mUSq02), Vector(mUSq10, mUSq11, mUSq12), Vector(mUSq20, mUSq21, mUSq22))),
      mDSq = Matrix(Vector(
        Vector(mDSq00, mDSq01, mDSq02), Vector(mDSq10, mDSq11, mDSq12), Vector(mDSq20, mDSq21, mDSq22))),
      mESq = Matrix(Vector(
        Vector(mESq00, mESq01, mESq02), Vector(mESq10, mESq11, mESq12), Vector(mESq20, mESq21, mESq22)))
    )

    val vector = Vector(
      m1, m2, m3,
      aU00, aU01, aU02, aU10, aU11, aU12, aU20, aU21, aU22,
      aD00, aD01, aD02, aD10, aD11, aD12, aD20, aD21, aD22,
      aE00, aE01, aE02, aE10, aE11, aE12, aE20, aE21, aE00,
      b, mHuSq, mHdSq,
      mQSq00, mQSq01, mQSq02, mQSq10, mQSq11, mQSq12, mQSq20, mQSq21, mQSq22,
      mLSq00, mLSq01, mLSq02, mLSq10, mLSq11, mLSq12, mLSq20, mLSq21, mLSq22,
      mUSq00, mUSq01, mUSq02, mUSq10, mUSq11, mUSq12, mUSq20, mUSq21, mUSq22,
      mDSq00, mDSq01, mDSq02, mDSq10, mDSq11, mDSq12, mDSq20, mDSq21, mDSq22,
      mESq00, mESq01, mESq02, mESq10, mESq11, mESq12, mESq20, mESq21, mESq22
    )

    val vectorable = implicitly[Vectorable[MSSMSoftParams]]

    "be generated from vector" in {
      vectorable.fromVector(vector) mustEqual params
    }

    "be converted to vector" in {
      vectorable.toVector(params) mustEqual vector
    }

  }

}
