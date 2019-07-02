package kdkocev.simplytypedlabmdacalculus
import org.specs2.mutable.Specification

class ExpressionSpecs extends Specification {
  import Expression._
  object Sigma extends Type {
    override def toString: String = "σ"
  }
  object Rho extends Type {
    override def toString: String = "ρ"
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
    "λx.λx.x =a= λy.λy.y" in {
      val x = Variable('x, Sigma)
      val y = Variable('y, Sigma)
      val term1 = Abstraction(x, Abstraction(x, x))
      val term2 = Abstraction(y, Abstraction(y, y))

      term1.isAlphaEquivalent(term2) mustEqual true
    }
    "λx:ρ λx:σ x:ρ⇒σ y:ρ[y:ρ → x:ρ] = λ0:ρ λx:σ x:ρ⇒σ x:ρ" in {
      val zr = Variable(Symbol("0"), Rho)
      val xr = Variable('x, Rho)
      val xs = Variable('x, Sigma)
      val xrs = Variable('x, To(Rho, Sigma))
      val yr = Variable('y, Rho)
      val term = Abstraction(xr, Abstraction(xs, Application(xrs, yr)))
      val result = Abstraction(zr, Abstraction(xs, Application(xrs, xr)))

      substitution(term, yr, xr) mustEqual result
    }
  }
}
