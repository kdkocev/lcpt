package kdkocev.basic

import org.specs2.mutable.Specification

class ExpressionSpecs extends Specification {
  import Tooling._

  def test(expr: Expression): Expression = ???
//    renameBoundVariables(expr, find, replace)

  val find = Variable(Symbol("x"))
  val replace = Variable('j)

  "Expression.renameBoundVariables" >> {



    "Not rename single variables" in {
      test(find) mustEqual find
    }

    "Rename abstraction with bound variable correctly" in {
      val o = Abstraction(find, Application(find, Variable('y)))
      test(o) mustEqual Abstraction(replace, Application(replace, Variable('y)))
    }

    "Rename abstraction with bound variables correctly" in {
      val o = Abstraction(find, Application(find, replace))

      val res = test(o) match {
        case Abstraction(`replace`, Application(`replace`, x)) if x != `replace` =>
          true
        case _ => false
      }

      res mustEqual true
    }

    "Rename all variables at once" in {
      val o = Abstraction(
        Variable('x),
        Abstraction(
          Variable('y),
          Application(Variable('x), Variable('y)))
      )
      renameAll(o, List(Variable('a), Variable('b))) mustEqual Abstraction(
        Variable('a),
        Abstraction(
          Variable('b),
          Application(Variable('a), Variable('b)))
      )
    }

    "Alpha equivalence" in {
      val o1 = Abstraction(
        Variable('x),
        Abstraction(
          Variable('y),
          Application(Variable('x), Variable('y)))
      )

      val o2 = Abstraction(
        Variable('a),
        Abstraction(
          Variable('b),
          Application(Variable('a), Variable('b)))
      )

      isAlphaEquivalent(o1, o2) mustEqual true
    }

    "rename bounded variables" in {
      val o1 = Abstraction(
        Variable('x),
        Abstraction(
          Variable('y),
          Application(Variable('x), Variable('y)))
      )

      val o2 = Abstraction(
        Variable('a),
        Abstraction(
          Variable('b),
          Application(Variable('a), Variable('b)))
      )

      renameBoundedVariables(o1, Variable('x), Variable('z)) mustEqual o2
    }

  }
}
