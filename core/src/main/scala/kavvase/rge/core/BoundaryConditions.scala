package kavvase.rge.core

trait BoundaryConditions[T <: GaugeTheory[A], A] {

  def lowEnergyBC(theory: T): T

  def highEnergyBC(theory: T): T

}

object BoundaryConditions {

  implicit def MSSMBoundaryConditions[A](implicit e: Numeric[A]): BoundaryConditions[MSSM[A], A] = {
    new BoundaryConditions[MSSM[A], A] {

      def lowEnergyBC(theory: MSSM[A]): MSSM[A] = ???

      def highEnergyBC(theory: MSSM[A]): MSSM[A] = ???

    }
  }

}