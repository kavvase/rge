package kavvase.rge.core

import org.specs2.mutable.Specification

class NumericSyntaxSpec extends Specification {

  "numeric" should {

    import kavvase.rge.core.numeric._

    "support power method" in {
      2.power(0) mustEqual 1
      2.power(3) mustEqual 8
      0.3.power(2) mustEqual 0.09
    }

  }

}
