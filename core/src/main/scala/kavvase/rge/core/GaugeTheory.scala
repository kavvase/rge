package kavvase.rge.core

import kavvase.rge.core.math.Matrix

sealed trait GaugeTheory[A]

case class MSSM[A](
  g1: A,
  g2: A,
  g3: A,
  yU: Matrix[A],
  yD: Matrix[A],
  yE: Matrix[A],
  mu: A,
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
) extends GaugeTheory[A]

