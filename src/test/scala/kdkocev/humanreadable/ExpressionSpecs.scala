package kdkocev.humanreadable

import org.specs2.mutable.Specification

class ExpressionSpecs extends Specification {
  "Expression" should {
    "construct" in {
      "Variables" in {
        Variable('x) must not(throwAn[Exception])
      }
    }
  }
}
