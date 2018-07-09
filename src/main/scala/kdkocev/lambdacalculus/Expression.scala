package kdkocev.lambdacalculus

trait Expression {
  def apply(f: Expression.Action): Expression = f(this)
  def isAlphaEquivalentTo(expr: Expression): Boolean = {
    import Expression._
    // TODO: make the possible variables an endless lazy-evaluated stream
    // All the characters are enough for our purposes
    val possibleVariables = ('a' to 'z').map(x => Variable(Symbol(x.toString))).toSet

    // Rename all the bound variables
    val valuesToRenameWith = ((possibleVariables diff V(this).toSet) diff V(expr).toSet)
      .toList
      .sortBy(v => v.s.toString)

    val valuesToRenameExpr1 = BV(this)
    val renamedBV1 = valuesToRenameExpr1.zip(valuesToRenameWith).foldLeft(this){
      case (ex, (find, replace)) => ex(find to replace)
    }

    val valuesToRenameExpr2 = BV(expr)
    val renamedBV2 = valuesToRenameExpr2.zip(valuesToRenameWith).foldLeft(expr){
      case (ex, (find, replace)) => ex(find to replace)
    }

    renamedBV1 == renamedBV2
  }

  def <=> : (Expression => Boolean) = isAlphaEquivalentTo
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
  def to (v: Variable) = Renaming(this, v)
}
case class Application(m1: Expression, m2: Expression) extends Expression
case class Abstraction(v: Variable, body: Expression) extends Expression


case class Substitution(find: Variable, replace: Expression) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._
    def iter(expr: Expression): Expression = expr match {
      case `find` => replace
      case y: Variable => y
      case Application(x, y) => Application(iter(x), iter(y))
      case Abstraction(`find`, body) => Abstraction(find, body)
      case Abstraction(head, body) =>
        if(FV(replace).contains(head)) {
          // Throwing an exception instead of returning an Option because this should
          // break the program execution instead of silently failing
          throw new Exception(s"Substitution($find, $replace) not defined for $body")
        } else {
          Abstraction(head, iter(body))
        }
    }
    iter(expression)
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

// Rename the first occurrence of `find` to `replace`
case class Renaming(find: Variable, replace: Variable) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    import Expression._
    def iter(expr: Expression, bound: List[Variable], renameInApplication: Boolean): Expression = expr match {
      case `find` => if(bound.contains(find)) `replace` else `find`
      case x: Variable if x != find => x
      case Application(x, y) =>
        // If the first occurrence of `find` hasnt been reached rename
        // only one of the branches of the Application
        if(renameInApplication) {
          Application(iter(x, bound, renameInApplication), iter(y, bound, renameInApplication))
        } else {
          if(BV(x).contains(find)) {
            Application(iter(x, bound, renameInApplication), y)
          } else {
            Application(x, iter(y, bound, renameInApplication))
          }
        }
      case Abstraction(`find`, body) =>
        if(BV(expr).contains(replace)) {
          throw new Exception(s"Cannot rename λ$find.$expr[$find -> $replace]")
        }
        // Start renaming all branches of applications
        Abstraction(`replace`, iter(body, find :: bound, true))
      case Abstraction(head, body) if head != find => Abstraction(head, iter(body, bound, renameInApplication))
    }
    if(FV(expression).contains(replace)) {
      throw new Exception(s"Cannot rename $expression[$find -> $replace]")
    }
    iter(expression, Nil, false)
  }
}
