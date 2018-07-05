package kdkocev.humanreadable

trait Expression {
  def apply(f: Expression.Action): Expression = f(this)
  def isAlphaEquivalentTo(expr: Expression): Boolean = true

  def <=> : (Expression => Boolean) = isAlphaEquivalentTo
}
object Expression {
  type Action = (Expression => Expression)

  // Implicit type casting from Symbol to Variable
  implicit def symbolToVar(s: Symbol): Variable = Variable(s)

  def lam(x: Variable, y: Expression) = Abstraction(x, y)
  def app(x: Expression, y: Expression) = Application(x, y)

  // Bound variables
  def BV(expression: Expression): Set[Variable] = expression match {
    case x: Variable => Set()
    case Application(x, y) => BV(x) ++ BV(y)
    case Abstraction(head, body) => BV(body) + head
  }

  // All variables
  def V(expression: Expression): Set[Variable] = expression match {
    case x: Variable => Set(x)
    case Application(x, y) => V(x) ++ V(y)
    case Abstraction(head, body) => V(body) + head
  }

  // Free variables
  def FV(expression: Expression): Set[Variable] = {
    def iter(expr: Expression, bound: List[Variable]): Set[Variable] = expr match {
      case x: Variable => if (bound.contains(x)) Set() else Set(x)
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

case class Renaming(find: Variable, replace: Variable) extends Expression.Action {
  def apply(expression: Expression): Expression = {
    def iter(expr: Expression, bound: List[Variable]): Expression = expr match {
      case `find` => if(bound.contains(find)) `replace` else `find`
      case x: Variable if x != find => x
      case Application(x, y) => Application(iter(x, bound), iter(y, bound))
      case Abstraction(`find`, body) => Abstraction(`replace`, iter(body, find :: bound))
      case Abstraction(head, body) if head != find => Abstraction(head, iter(body, bound))
    }
    iter(expression, Nil)
  }
}

object Main extends App {
  import Expression._

  val e = lam('x, app('y, 'x))
  val e1 = app(lam('x, app('x, 'z)), lam('x, lam('y, app('x, 'd))))

  println(e('y -> 'z))
  println(e('y ~> 'z))
  println(e('x to 'z))
//  println(BV(e))
//  println(V(e))
//  println(FV(e))
//  println(BV(e1))
//  println(V(e1))
//  println(FV(e1))

}