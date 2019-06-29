package kdkocev.simplytypedlabmdacalculus

trait Type
case class To[T1 <: Type, T2 <: Type](t1: T1, t2: T2) extends Type {
  override def toString: String = s"($t1 -> $t2)"
}

trait Expression {
  def typ: Type = this match {
    case v: Variable => v.t
    case Abstraction(v, b) => To(v.typ, b.typ)
    case Application(e1, e2) =>

      (e1.typ, e2.typ) match {
        case (To(t1, t2), t3) if t1 == t3 => t2
        case _ => throw new Error(s"Tried to apply $e2 to $e1 but types dont match")
      }
  }

  def allVariables(variables: Set[Variable] = Set()): Set[Variable] = this match {
    case v: Variable => variables + v
    case Application(e1, e2) => variables ++ e1.allVariables(Set()) ++ e2.allVariables(Set())
    case Abstraction(v, b) => b.allVariables(variables + v)
  }

  def freeVariables(free: Set[Variable] = Set(), bound: Set[Variable] = Set()): Set[Variable] = this match {
    case v: Variable if !bound.contains(v) => free + v
    case v: Variable if bound.contains(v) => free
    case Application(e1, e2) => free ++ e1.freeVariables(Set(), bound) ++ e2.freeVariables(Set(), bound)
    case Abstraction(v, b) => b.freeVariables(free, bound + v)
  }

  def boundVariables(bound: Set[Variable] = Set()): Set[Variable] = this match {
    case _: Variable => bound
    case Application(e1, e2) => bound ++ e1.boundVariables(Set()) ++ e2.boundVariables(Set())
    case Abstraction(v, b) => b.boundVariables(bound + v)
  }

  def substitute(from: Variable, to: Expression): Expression =
    Expression.softSubstitution(this, from, to)

  def rename(from: Symbol, to: Symbol): Expression =
    Expression.rename(this, from, to)

  def isAlphaEquivalent(other: Expression): Boolean =
    Expression.areAlphaEquivalent(this, other)

  def betaReduce(): Expression =
    Expression.betaReduce(this)
}

object Expression {
  // TODO make it so the substitution makes valid renamings instead of throwing an error.
  def softSubstitution(e: Expression, from: Variable, to: Expression): Expression = {
    if(from.typ != to.typ) {
      throw new Error("Cannot do substitution")
    } else {
      e match {
        case v: Variable if v == from => to
        case v: Variable if v != from => v
        case Application(e1, e2) => Application(softSubstitution(e1, from, to), softSubstitution(e2, from, to))
        case Abstraction(v, b) if v == from => Abstraction(v, b)
        case Abstraction(v, b) if v != from =>
          if(to.freeVariables().contains(v)) {
            // TODO: do valid renaming and complete the substitution
            throw new Error(s"Cannot substitute $from with $to in $b because $v is free in $to")
          } else {
            Abstraction(v, softSubstitution(b, from, to))
          }
      }
    }
  }

  def substitution(e: Expression, from: Variable, to: Expression): Expression = {
    if(from.typ != to.typ) {
      throw new Error("Cannot do the substitution")
    } else {
      e match {
        case v: Variable if v == from => to
        case v: Variable if v != from => v
        case Application(e1, e2) => Application(substitution(e1, from, to), substitution(e2, from, to))
        case Abstraction(v, b) if v == from => Abstraction(v, b)
        case Abstraction(v, b) if v != from =>
          if(to.freeVariables().contains(v)) {
            // Find the biggest number amongst the variable names and increment it by 1
            val newVal: Int = b.allVariables().filter(p =>
              p.symbol.name.forall(c => c.isDigit)
            ).foldLeft(-1)((max, f) =>
              if(max > f.symbol.name.toInt)
                max
              else
                f.symbol.name.toInt
            ) + 1

            substitution(rename(Abstraction(v, b), v.symbol, Symbol(newVal.toString)), from, to)
          } else {
            Abstraction(v, substitution(b, from, to))
          }
      }
    }
  }

  // Renaming of *bound variables* only. Free variables are left as they are
  // TODO: make sure that expressions are alpha equivalent after renaming
  def rename(expression: Expression, from: Symbol, to: Symbol): Expression = {
    // All pattern matches checks are explicit so they don't depend on ordering

    // Iterating without using substitution
    def iter_version1(expr: Expression, isBound: Boolean): Expression = expr match {
      case x: Variable if x.symbol == from && isBound => Variable(to, x.t)
      case x: Variable if x.symbol != from || !isBound => x
      case Application(e1, e2) => Application(iter_version1(e1, isBound), iter_version1(e2, isBound))
      case Abstraction(v, body) if v.symbol == to && isBound =>
        throw new Exception(s"Trying to rename $from to $to in $expr but $to is found in a bound position")

      case Abstraction(v, body) if v.symbol != from => Abstraction(v, iter_version1(body, isBound))
      case abs@Abstraction(v, body) if v.symbol == from =>

        if(abs.freeVariables().exists(v => v.symbol == to)) {
          throw new Error(s"Cannot rename $from with $to in $abs because $to is free in $expression")
        }

        Abstraction(Variable(to, v.t), iter_version1(body, true))
    }

    // We use substitution in this case
    def iter(expr: Expression): Expression = expr match {
      case x: Variable => x
      case Application(e1, e2) => Application(iter(e1), iter(e2))
      case Abstraction(v, body) if v.symbol != from => Abstraction(v, iter(body))
      case Abstraction(v, body) if v.symbol == from =>
        if(body.allVariables().exists(variable => variable.symbol == to)) {
          throw new Error(s"Cannot rename $from to $to in $body becaues $to is either bound or free in $body")
        } else {
          Abstraction(
            Variable(to, v.t),
            Expression.softSubstitution(body, Variable(from, v.t), Variable(to, v.t))
          )
        }
    }


    iter(expression)
  }

  def areAlphaEquivalent(e1: Expression, e2: Expression): Boolean = {
    // use only renaming of bound variables in both expressions and turn them into the same expression

    def iter(expr: Expression, excludedVariables: Set[Variable], bound: Set[Variable], iteration: Int): Expression = {
      // If the "iteration" number exists as a variable - increment it
      if(excludedVariables.exists(v => v.symbol == Symbol(iteration.toString))) {
        iter(expr, excludedVariables, bound, iteration + 1)
      } else {
        bound.toList match {
          case Nil => expr
          case (firstBound: Variable) :: restBound =>
            iter(rename(expr, firstBound.symbol, Symbol(iteration.toString)), excludedVariables, restBound.toSet, iteration + 1)
        }
      }
    }

    val excludedVariables = e1.allVariables() ++ e2.allVariables()
    val e1Renamed = iter(e1, excludedVariables, e1.boundVariables(), 0)
    val e2Renamed = iter(e2, excludedVariables, e2.boundVariables(), 0)

    println(e1Renamed, e2Renamed)

    e1Renamed == e2Renamed
  }

  def betaReduce(expression: Expression): Expression = expression match {
    case x: Variable => x
    case Application(Abstraction(v, b), e2) if v.typ == e2.typ => betaReduce(Expression.softSubstitution(b, v, e2))
    case Application(Abstraction(v, b), e2) if v.typ != e2.typ => throw new Error(s"Type mismatch in expression $expression. Expected ${v.typ} but found ${e2.typ}")
    // This constraint is removed for testing purposes
    // case Application(e1, e2) => throw new Error(s"Tried to apply $e2 to $e1 but the second one is not an abstraction.")
    // This constraint should be removed after testing
    case Application(e1, e2) => Application(betaReduce(e1), betaReduce(e2))
    case Abstraction(v, b) => Abstraction(v, betaReduce(b))
  }

  // Returns a list of errors
  def typeCheck(expression: Expression): List[String] = {
    def iter(expr: Expression, errors: List[String]): List[String] = expr match {
      case _: Variable => errors
      case Application(Abstraction(v, b), e2) if v.typ == e2.typ => errors ++ iter(b, Nil) ++ iter(e2, Nil)
      case Application(Abstraction(v, b), e2) if v.typ != e2.typ => errors ++ List(s"Expected ${v.typ} to equal ${e2.typ}") ++ iter(b, Nil) ++ iter(e2, Nil)
//      case Application(e1, e2) if e1.isInstanceOf[To[_,_]] && e1.asInstanceOf[To[_,_]].t1 == e2.typ => errors ++ iter(e1, Nil) ++ iter(e2, Nil)
//      case Application(e1, e2) if !e1.typ.isInstanceOf[To[_,_]] => errors ++ List(s"Tried to apply $e2 to $e1 but types are incompatible") ++ iter(e1, Nil) ++ iter(e2, Nil)
      case Application(e1, e2) => e1.typ match {
        case To(t1, _) if t1 == e2.typ => errors ++ iter(e1, Nil) ++ iter(e2, Nil)
        case _ => errors ++ List(s"Tried to apply ${e2.typ} to ${e1.typ} but types are incompatible") ++ iter(e1, Nil) ++ iter(e2, Nil)
      }
      case Abstraction(_, b) => iter(b, errors)
    }

    iter(expression, Nil)
  }
}

case class Variable(symbol: Symbol, t: Type) extends Expression
case class Application(e1: Expression, e2: Expression) extends Expression
case class Abstraction(v: Variable, body: Expression) extends Expression

object Main extends App {

  object Sigma extends Type {
    override def toString: String = "σ"
  }

  val x = Variable('x, Sigma)
  val y = Variable('y, Sigma)
  val z = Variable('z, Sigma)

  val abs1 = Abstraction(x, x)
  //  println(abs1.typ) // (σ -> σ)

  val app1 = Application(abs1, y)
  //  println(app1.typ) // σ

  val abs2 = Abstraction(Variable('x, To(Sigma, Sigma)), Application(Variable('x, To(Sigma, Sigma)), y))
  //  println(abs2.typ)

  //  val freevartest1 = Abstraction(x, Application(Application(Abstraction(y, y), y), Variable('t, Sigma)))
  //  println(freevartest1.boundVariables())
  //  println(freevartest1.freeVariables())
  //  println(freevartest1.allVariables())


  //  val test1 = Expression.substitution(Abstraction(z, Application(x, Abstraction(y, x))), x, y)
  //  println(test1)

  //    val test2 = Expression.rename(Abstraction(x, Application(Abstraction(z, z), y)), z.symbol, y.symbol)
  //    val test2 = Expression.rename(Abstraction(x, Application(x, y)), x.symbol, y.symbol)
  //    println(test2)

    val zero = Variable(Symbol("0"), Sigma)
    val one = Variable(Symbol("1"), Sigma)
  //  val test3 = Expression.areAlphaEquivalent(
  //    Application(Abstraction(one, one), x),
  //    Application(Abstraction(zero, zero), x)
  //  )
  //  println(test3)

  //  val test4 = Expression.rename(Abstraction(x, Abstraction(y, Abstraction(y, Application(x, Abstraction(x, x))))), x.symbol, z.symbol)
  //  print(test4)

  //  val test5 = Expression.areAlphaEquivalent(
  //    Abstraction(x, Application(x, y)),
  //    Expression.substitution(Abstraction(z, Application(z, x)), x, y)
  //  )
  //  print(test5)

  //  val test6 = Expression.betaReduce(Application(Abstraction(x, x), Abstraction(y, y)))
  //  println(test6)

  //  val test7 = Expression.typeCheck(Application(Abstraction(Variable('u, To(Sigma, Sigma)), Application(Variable('u, To(Sigma, Sigma)), z)), Abstraction(x, x)))
  //  println(test7)

  //  val test8 = Expression.typeCheck(Application(Abstraction(Variable('u, To(Sigma, Sigma)), Application(Variable('u, To(Sigma, Sigma)), Variable('k, To(Sigma, Sigma)))), Abstraction(x, x)))
  //  println(test8)

  val test9 = Expression.substitution(Abstraction(one, Application(one, x)), x, one)
  println(test9)
}