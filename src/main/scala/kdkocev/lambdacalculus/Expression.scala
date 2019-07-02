package kdkocev.lambdacalculus

trait Expression {
  def apply(f: Expression.Action): Expression = f(this)

  // Important: See tests for documentation
  def normalize: Expression = {
    import Expression._
    def iter(expr: Expression): (Expression, Int) = expr match {
      case x: Variable => (x, 0)

      case Application(m1, m2) =>
        val (p1, n1) = iter(m1)
        val (p2, n2) = iter(m2)
        (Application(p1, p2), Math.max(n1, n2))

      case Abstraction(x, body) =>
        val (p, n) = iter(body)

        def getNumToReplaceWith(numToRepWi: Int): Int = {
          if(V(Abstraction(x, p)).contains(Variable(Symbol(numToRepWi.toString)))) {
            getNumToReplaceWith(numToRepWi + 1)
          } else numToRepWi
        }

        val numToReplaceWith = getNumToReplaceWith(n)

        // Substitute x with n in the body
        val symbolToReplaceWith = Variable(Symbol(numToReplaceWith.toString))
        val res = Abstraction(x, p)(Renaming(x, symbolToReplaceWith))
        (res, n + 1)
    }

    // Rename variables with symbols that are among the numbers we're going to rename with later
    // TL;DR: find variables with name that is digit between 0 and variables.size. rename them
    val variables = V(this)
    val digitVariables = variables.filter(x => x.s.name.forall(c => c.isDigit))

    if(digitVariables.nonEmpty) {
      val maxNumber = digitVariables.map(_.s.name).max

      val expr = digitVariables.zipWithIndex.foldLeft(this) {
        case (res, (x, currentIndex)) =>
          res(Renaming(x, Symbol(maxNumber + currentIndex)))
      }

      iter(expr)._1
    } else {
      iter(this)._1
    }
  }

  def isAlphaEquivalentTo(expr: Expression): Boolean = this.normalize == expr.normalize

  def <=> : (Expression => Boolean) = isAlphaEquivalentTo
  def =:= : (Expression => Boolean) = isAlphaEquivalentTo
}
object Expression {
  type Action = (Expression => Expression)

  // Implicit type casting from Symbol to Variable
  implicit def symbolToVar(s: Symbol): Variable = Variable(s)

  def lam(x: Variable, y: Expression) = Abstraction(x, y)
  def app(x: Expression, y: Expression) = Application(x, y)

  // Bound variables
  def BV(expression: Expression): List[Variable] = expression match {
    case x: Variable => List()
    case Application(x, y) => BV(x) ++ BV(y)
    case Abstraction(head, body) => head :: BV(body)
  }

  // All variables
  def V(expression: Expression): List[Variable] = expression match {
    case x: Variable => List(x)
    case Application(x, y) => V(x) ++ V(y)
    case Abstraction(head, body) => head :: V(body)
  }

  // Free variables
  def FV(expression: Expression): List[Variable] = {
    def iter(expr: Expression, bound: List[Variable]): List[Variable] = expr match {
      case x: Variable => if (bound.contains(x)) List() else List(x)
      case Application(x, y) => iter(x, bound) ++ iter(y, bound)
      case Abstraction(head, body) => iter(body, head :: bound)
    }
    iter(expression, Nil)
  }
}

case class Variable(s: Symbol) extends Expression {
  def -> (expr: Expression) = Substitution(s, expr)
  def ~> (expr: Expression) = Substitution2(s, expr)
  def --> (expr: Expression) = Substitution3(s, expr)
  def to (v: Variable) = Renaming(this, v)
}
case class Application(m1: Expression, m2: Expression) extends Expression
case class Abstraction(v: Variable, body: Expression) extends Expression


case class Substitution(find: Variable, replace: Expression) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._
    def iter(expr: Expression, symbolToReplaceWith: Int): Expression = expr match {
      case `find` => replace
      case y: Variable => y
      case Application(x, y) => Application(iter(x, symbolToReplaceWith), iter(y, symbolToReplaceWith))
      case Abstraction(`find`, body) => Abstraction(find, body)
      case Abstraction(head, body) =>
        // Rename values if needed
        if(FV(replace).contains(head)) {
          val renamedBody = Abstraction(head, body)(Renaming(head, Symbol(symbolToReplaceWith.toString)))
          iter(renamedBody, symbolToReplaceWith + 1)
        } else {
          Abstraction(head, iter(body, symbolToReplaceWith))
        }
    }
    iter(expression, 0)
  }
}

case class Substitution2(find: Variable, replace: Expression) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._
    def iter(expr: Expression): Expression = expr match {
      case `find` => replace
      case y: Variable => y
      case Application(x, y) => Application(iter(x), iter(y))
      case Abstraction(`find`, body) => Abstraction(find, body)
      case Abstraction(head, body) =>
        if ((FV(replace) intersect BV(expression)).isEmpty) {
          Abstraction(head, iter(body))
        } else {
          // Throwing exception because this error should fail the entire program
          throw new Exception(s"Substitution2($find, $replace) not defined for $expr in $expression")
        }
    }
    iter(expression)
  }
}

case class Substitution3(find: Variable, replace: Expression) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._
    def iter(expr: Expression): Expression = expr match {
      case x: Variable if x == find => replace
      case y: Variable if y != find => y
      case Application(x, y) => Application(iter(x), iter(y))
      case Abstraction(v, b) if v == find => Abstraction(v, b)
      case Abstraction(v, b) if v != find =>
        if(FV(replace).contains(v)) {
          // Find the biggest number amongst the variable names and increment it by 1
          val newVal: Int = V(b).filter(p =>
            p.s.name.forall(c => c.isDigit)
          ).foldLeft(-1)((max, f) =>
            if(max > f.s.name.toInt)
              max
            else
              f.s.name.toInt
          ) + 1

          val renamedBody = Abstraction(v, b)(Renaming(v.s, Symbol(newVal.toString)))
          iter(renamedBody)
        } else {
          Abstraction(v, iter(b))
        }
    }

    iter(expression)
  }
}

case class Renaming(find: Variable, replace: Variable) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._

    // Replaces all occurrences of find with replace blindly
    // Even in the event (lx(lx.xy))[x to z] = (lz(lz.zy))
    def replaceAll(expr: Expression, find: Variable, replace: Variable): Expression = expr match {
      case `find` => `replace`
      case y: Variable if y != `find` => y
      case Application(m1, m2) =>
        Application(replaceAll(m1, find, replace), replaceAll(m2, find, replace))
      case Abstraction(`find`, body) => Abstraction(replace, replaceAll(body, find, replace))
      case Abstraction(x, body) if x != find => Abstraction(x, replaceAll(body, find, replace))
    }

    def iter(expr: Expression): Expression = expr match {
      case x: Variable => x
      case Application(m1, m2) => Application(iter(m1), iter(m2))
      case Abstraction(`find`, body) =>
        // The `replace` is a variable in the body of an abstraction over `find`
        if(V(body).contains(`replace`)) {
          throw new Exception(s"Cannot rename $find to $replace in $body")
        }

        Abstraction(replace, replaceAll(body, find, replace))
      case Abstraction(x, body) if x != find =>
        Abstraction(x, iter(body))
    }

    iter(expression)
  }
}