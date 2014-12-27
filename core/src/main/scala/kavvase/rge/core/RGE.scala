package kavvase.rge.core

sealed trait RGE[T <: GaugeTheory[A], A, B <: LoopOrder] {

  def susyRGE(theory: T): T#Susy[A]

  def softRGE(theory: T): T#Soft[A]

}

object RGE {

  implicit def MSSMOneLoopRGE[A](implicit e: Numeric[A], const: Constants[A]): RGE[MSSM[A], A, OneLoop] = {
    new RGE[MSSM[A], A, OneLoop] {

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
