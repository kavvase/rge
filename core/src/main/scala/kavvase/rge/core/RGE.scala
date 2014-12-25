package kavvase.rge.core

sealed trait RGE[A <: GaugeTheory[B, Susy, Soft], B, Susy[_], Soft[_], C <: LoopOrder] {

  def susyRGE(theory: A): Susy[B]

  def softRGE(theory: A): Soft[B]

}

object RGE {

  implicit def MSSMOneLoopRGE[A](implicit
                                 e: Numeric[A],
                                 const: Constants[A]): RGE[MSSM[A], A, MSSMSusyParams, MSSMSoftParams, OneLoop] = {
    new RGE[MSSM[A], A, MSSMSusyParams, MSSMSoftParams, OneLoop] {

      def susyRGE(theory: MSSM[A]): MSSMSusyParams[A] = {
        MSSMSusyParams[A](
          g1 = ???,
          g2 = ???,
          g3 = ???,
          yU = ???,
          yD = ???,
          yE = ???,
          mu = ???
        )
      }

      def softRGE(theory: MSSM[A]): MSSMSoftParams[A] = {
        MSSMSoftParams(
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

    }
  }

}

sealed trait LoopOrder

case class OneLoop() extends LoopOrder
