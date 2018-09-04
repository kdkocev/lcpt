package kdkocev.lambdacalculus

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
        "λx.x[x to y] == λy.y" in {
          lam('x, 'x)('x to 'y) mustEqual lam('y, 'y)
        }
        "λx.xy[x to z] == λz.zy" in {
          lam('x, app('x, 'y))('x to 'z) mustEqual lam('z, app('z, 'y))
        }
        "λx.yx[x to z] == λz.yz" in {
          lam('x, app('y, 'x))('x to 'z) mustEqual lam('z, app('y, 'z))
        }
        "λxλy.x[x to z] == λzλy.z" in {
          lam('x, lam('y, 'x))('x to 'z) mustEqual lam('z, lam('y, 'z))
        }
        "λxλy.x[t to z] == λxλy.x" in {
          lam('x, lam('y, 'x))('t to 'z) mustEqual lam('x, lam('y, 'x))
        }
        "λyλx.y[x to z] == λyλz.y" in {
          lam('y, lam('x, 'y))('x to 'z) mustEqual lam('y, lam('z, 'y))
        }
        "λx.xy[x to y] throw error" in {
          lam('x, app('x, 'y))('x to 'y) must throwAn[Exception]
        }
        "λx.(x(λy.y))[x to y] throw error" in {
          lam('x, app('x, lam('y, 'y)))('x to 'y) must throwAn[Exception]
        }
        "λx.(x(λz.zy))[x to y] throw error" in {
          lam('x, app('x, lam('z, app('z, 'y))))('x to 'y) must throwAn[Exception]
        }
        "λy.xy[x to z] == λy.xy (not changed)" in {
          val expr = lam('y, app('x, 'y))
          expr('x to 'z) mustEqual expr
        }
        "(λx.xy)(λx.xy)[x to z] == (λz.zy)(λz.zy)" in {
          val expr = app(lam('x, app('x, 'y)), lam('x, app('x, 'y)))
          expr('x to 'z) mustEqual app(lam('z, app('z, 'y)), lam('z, app('z, 'y)))
        }
        "λx.λx.x[x to y] == λy.λy.y" in {
          val expr = lam('x, lam('x,'x))
          expr('x to 'y) mustEqual lam('y, lam('y, 'y))
        }
        "λy.λx.x[x to y] == λy.λy.y" in {
          val expr = lam('y, lam('x,'x))
          expr('x to 'y) mustEqual lam('y, lam('y, 'y))
        }
        "λx.λy.x[x to y] == λx.λy.x EXCEPTION" in {
          val expr = lam('x, lam('y,'x))
          expr('x to 'y) must throwAn[Exception]
        }
      }
    }
    "Substitution [x -> y]" in {
      val l1 = lam('x, 'x)
      "substitute free variables" in {
        "x[x -> λy.y] == λy.y" in {
          Variable('x)('x -> l1) mustEqual l1
        }
        "(λx.x)(λy.x)[x -> z] == (λx.x)(λy.z)" in {
          app(lam('x, 'x), lam('y, 'x))('x -> 'z) mustEqual app(lam('x, 'x), lam('y, 'z))
        }
      }
    }

    "Alpha equivalence α=" in {
      "normalize" in {

        "λx.λx.x normalize == λ1.λ0.0" in {
          val expr = lam('x, lam('x,'x))
          val result = lam(Symbol("1"), lam(Symbol("0"), Symbol("0")))
          expr.normalize mustEqual result
        }
        "λy.λx.x normalize == λ1.λ0.0" in {
          val expr = lam('x, lam('x,'x))
          val result = lam(Symbol("1"), lam(Symbol("0"), Symbol("0")))
          expr.normalize mustEqual result
        }
        "λx.λy.z normalize == λ1.λ0.z" in {
          val expr = lam('x, lam('y, 'z))
          val result = lam(Symbol("1"), lam(Symbol("0"), 'z))
          expr.normalize mustEqual result
        }
        "y(λx.λz.tx)(λy.λx.y(λz.λy.zty)(λd.d)) normalize == y(λ1.λ0.0t)(λ3.λ2.3(λ1.λ0.103)(λ0.0))" in {
          val expr = app(
            app(
              'y,
              lam('x, lam('z, app('t, 'x)))
            ),
            lam('y,
              lam('x,
                app(
                  app('y,
                    lam('z,
                      lam('y, app(app('z, 't), 'y))
                    )
                  ),
                  lam('d, 'd)
                )
              )
            )
          )

          val result = app(
            app(
              'y,
              lam(Symbol("1"), lam(Symbol("0"), app('t, Symbol("1"))))
            ),
            lam(Symbol("3"),
              lam(Symbol("2"),
                app(
                  app(Symbol("3"),
                    lam(Symbol("1"),
                      lam(Symbol("0"), app(app(Symbol("1"), 't), Symbol("0")))
                    )
                  ),
                  lam(Symbol("0"), Symbol("0"))
                )
              )
            )
          )

          expr.normalize mustEqual result
        }

      }
      "λx.λx.x =a= λy.λy.y" in {
        val expr = lam('x, lam('x,'x))
        val result = lam('y, lam('y, 'y))
        expr =:= result mustEqual true
      }
      "λy.λx.x =a= λy.λy.y" in {
        val expr = lam('x, lam('x,'x))
        val result = lam('y, lam('y, 'y))
        expr =:= result mustEqual true
      }
      "λx.λy.y =a= λy.λx.x" in {
        val expr = lam('x, lam('y,'y))
        val result = lam('y, lam('x, 'x))
        expr =:= result mustEqual true
      }
      "λx.λy.z =a= λy.λy.z" in {
        val expr = lam('x, lam('y, 'z))
        val result = lam('y, lam('y, 'z))
        expr =:= result mustEqual true
      }

    }
    "Substitution" in {
      import Expression._
      "x[x -> y] := y" in {
        Variable('x)('x -> 'y) =:= Variable('y) mustEqual true
      }
      "x[x -> λy.y] := λy.y" in {
        Variable('x)('x -> lam('y, 'y)) =:= lam('y, 'y) mustEqual true
      }
      "x[x -> yx] := yx" in {
        Variable('x)('x -> app('y, 'x)) =:= app('y, 'x) mustEqual true
      }
      "λx.x[x -> y] := λx.x" in {
        lam('x, 'x)('x -> 'y) =:= lam('x, 'x) mustEqual true
      }
      "λx.xy[y -> t] := λx.xt" in {
        lam('x, app('x, 'y))('y -> 't) =:= lam('x, app('x, 't)) mustEqual true
      }
      "λx.xy[y -> x] := λz.zx RENAME" in {
        lam('x, app('x, 'y))('y -> 'x) =:= lam('z, app('z, 'x)) mustEqual true
      }
      "(λx.x)(λy.y)[z -> t] := (λx.x)(λy.y)" in {
        val expr = app(lam('x, 'x),lam('y, 'y))

        expr('z -> 't) =:= expr mustEqual true
      }
      "(λx.xy)y[y -> λx.x] := (λx.x(λx.x))(λx.x)" in {
        val expr = app(lam('x, app('x, 'y)), 'y)
        val result = app(lam('x, app('x, lam('x, 'x))), lam('x, 'x))

        expr('y -> lam('x, 'x)) =:= result mustEqual true
      }
      "(λx.xy)(λy.y)yx[y -> x] := (λy.yx)(λy.y)xx RENAME" in {
        val expr = app(
          app(
            app(
              lam('x, app('x, 'y)),
              lam('y, 'y)
            ),
            'y
          ),
          'x
        )
        val result = app(
          app(
            app(
              lam('y, app('y, 'x)),
              lam('y, 'y)
            ),
            'x
          ),
          'x
        )
        expr('y -> 'x) =:= result mustEqual true
      }
    }
  }
}
