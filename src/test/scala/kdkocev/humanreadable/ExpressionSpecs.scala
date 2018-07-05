package kdkocev.humanreadable

import org.specs2.mutable.Specification

class ExpressionSpecs extends Specification {
  import Expression._
  "Expression" should {
    "construct" in {
      "Variables" in {
        // x
        Variable('x) must not(throwAn[Exception])
      }
      "Application" in {
        // xy
        Application('x, 'y) must not(throwAn[Exception])
      }
      "Abstraction" in {
        // λx.x
        Abstraction('x, 'x) must not(throwAn[Exception])
        // λx.yx
        Abstraction('x, Application('y, 'x)) must not(throwAn[Exception])
      }
      "Substitution" in {
        // Creates a substitution to later be used on an expression
        // [x -> y]
        Substitution('x, 'y) must not(throwAn[Exception])
        // [x -> λy.y]
        Substitution('x, Abstraction('y, 'y)) must not(throwAn[Exception])

        // Syntactic sugar for creating substitution is to call `->` on a variable
        // [x -> y]
        'x -> 'y mustEqual Substitution('x, 'y)
        // [x -> λy.y]
        'x -> Abstraction('y, 'y) mustEqual Substitution('x, Abstraction('y, 'y))
      }
      "Substitution 2" in {
        // There is a second way that we can define a substitution `~>`
        // [x ~> y]
        Substitution2('x, 'y) must not(throwAn[Exception])
        // [x ~> λy.y]
        Substitution2('x, Abstraction('y, 'y)) must not(throwAn[Exception])

        // Syntactic sugar for creating substitution is to call `->` on a variable
        // [x ~> y]
        'x ~> 'y mustEqual Substitution2('x, 'y)
        // [x ~> λy.y]
        'x ~> Abstraction('y, 'y) mustEqual Substitution2('x, Abstraction('y, 'y))
      }
      "Renaming" in {
        // Creates a Variant to later be used on an expression
        Renaming('x, 'y) must not(throwAn[Exception])
        // Syntactic sugar for creating a Variant
        'x to 'y mustEqual Renaming('x, 'y)
      }
    }
  }
}
