package kdkocev.simplytypedlabmdacalculus
import org.specs2.mutable.Specification

class ExpressionSpecs extends Specification {
  object Sigma extends Type {
    override def toString: String = "σ"
  }

  "Expression" should {
    "construct variables" in {
      val x = Variable('x, Sigma)

      x.typ mustEqual Sigma
    }

    "construct Abstractions" in {
      val x = Variable('x, Sigma)

      val abs = Abstraction(x, x)

      abs.typ mustEqual To(Sigma, Sigma)
    }

    "construct Applications" in {
      val x = Variable('x, Sigma)
      val y = Variable('y, Sigma)

      val abs = Abstraction(x, x)
      val app = Application(abs, y)

      abs.typ mustEqual To(Sigma, Sigma)
      app.typ mustEqual Sigma
    }

    "construct complex applications" in {
      val y = Variable('y, Sigma)

      val abs1 = Abstraction(Variable('x, To(Sigma, Sigma)), Application(Variable('x, To(Sigma, Sigma)), y))

      abs1.typ mustEqual To(To(Sigma, Sigma), Sigma)
    }

    "throw type errors" in {
      val y = Variable('y, Sigma)

      val abs1 = Abstraction(Variable('x, To(Sigma, Sigma)), Application(Variable('x, To(Sigma, Sigma)), Abstraction(y, y)))

      abs1.typ must throwAn[Error]
    }
  }
}
