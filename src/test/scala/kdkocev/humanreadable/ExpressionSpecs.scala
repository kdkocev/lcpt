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

    "Rename" in {
      "not rename a free variable" in {
        Variable('x)('x to 'y) mustEqual Variable('x)
      }
      "not rename an application of free variables" in {
        app('x, 'y)('x to 't) mustEqual app('x, 'y)
      }
      "rename bound variables" in {
        "λx.x[x -> y] == λy.y" in {
          lam('x, 'x)('x to 'y) mustEqual lam('y, 'y)
        }
        "λx.xy[x -> z] == λz.zy" in {
          lam('x, app('x, 'y))('x to 'z) mustEqual lam('z, app('z, 'y))
        }
        "λx.yx[x -> z] == λz.yz" in {
          lam('x, app('y, 'x))('x to 'z) mustEqual lam('z, app('y, 'z))
        }
        "λxλy.x[x -> z] == λzλy.z" in {
          lam('x, lam('y, 'x))('x to 'z) mustEqual lam('z, lam('y, 'z))
        }
        "λyλx.y[x -> z] == λyλz.y" in {
          lam('y, lam('x, 'y))('x to 'z) mustEqual lam('y, lam('z, 'y))
        }
        "λx.xy[x -> y] throw error" in {
          lam('x, app('x, 'y))('x to 'y) must throwAn[Exception]
        }
        "λx.(x(λy.y))[x -> y] throw error" in {
          lam('x, app('x, lam('y, 'y)))('x to 'y) must throwAn[Exception]
        }
        "λx.(x(λz.zy))[x -> y] throw error" in {
          lam('x, app('x, lam('z, app('z, 'y))))('x to 'y) must throwAn[Exception]
        }
        "λy.xy[x -> z] == λy.xy (not changed)" in {
          val expr = lam('y, app('x, 'y))
          expr('x to 'z) mustEqual expr
        }
      }
    }
  }
}
