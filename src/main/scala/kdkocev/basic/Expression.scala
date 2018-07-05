package kdkocev.basic

// Lazy evaluated expression
sealed trait Expression
final case class Variable(symbol: Symbol) extends Expression {
  override def toString: String = symbol.name
}
final case class Abstraction(x: Variable, y: Expression) extends Expression {
  override def toString: String = s"/$x.$y"
}
final case class Application(x: Expression, y: Expression) extends Expression {
  override def toString: String = s"($x)($y)"
}


object Tooling {

  def getRandomVariable: Variable = {
    val possibleVariables: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r, 's, 't, 'u, 'v, 'w, 'x, 'y, 'z)
    val r = scala.util.Random
    Variable(possibleVariables(r.nextInt(possibleVariables.size)))
  }


  // literaly replace with no checks
  // only to be used in renameBoundedVariables
  private def replaceAll(expr: Expression, find: Variable, replace: Variable): Expression = {
    expr match {
      case `find` => replace
      case y: Variable if y != find => y
      case Application(m1, m2) =>
        Application(replaceAll(m1, find, replace), replaceAll(m2, find, replace))
      case Abstraction(`find`, back) =>
        Abstraction(replace, replaceAll(back, find, replace))
      case Abstraction(front, back) if front != find =>
        Abstraction(front, replaceAll(back, find, replace))
    }
  }

  def renameBoundedVariables(expr: Expression, find: Variable, replace: Variable): Expression = {
    expr match {
      case x: Variable => x
      case Application(m1, m2) =>
        Application(renameBoundedVariables(m1, find, replace), renameBoundedVariables(m2, find, replace))
      case Abstraction(`find`, body) =>
        if(allVariables(body).contains(replace)) {
          throw new Exception(s"Cannot replace $find with $replace in ${Abstraction(find, body)}")
        } else {
          Abstraction(replace, replaceAll(body, find, replace))
        }
    }
  }




  def substitute(expr: Expression, find: Variable, replace: Expression): Expression =
    expr match {
      case `find` => replace
      case x: Variable if x != find => x
      case Application(x, y) => Application(substitute(x, find, replace), substitute(y, find, replace))

      case x@Abstraction(`find`, _) => x

      case Abstraction(front, back) if front != find =>

        if(freeVariables(replace).contains(front)) {
          substitute(expr, find, substitute(replace, front, getRandomVariable))
        } else {
          Abstraction(front, substitute(back, find, replace))
        }

      case x => x
    }






//  def renameBoundVariables(expr: Expression, find: Variable, replace: Variable, replaceFree: Boolean = false): Expression = expr match {
//    case Abstraction(front, back) if !replaceFree && freeVariables(back).contains(replace) =>
//      renameBoundVariables(
//        Abstraction(front,
//          renameBoundVariables(back, replace, getRandomVariable ,replaceFree=true)
//        ),
//        find,
//        replace
//      )
//
//    case `find` if replaceFree => replace
//
//    case x: Variable => x
//
//    case Application(x, y) =>
//      Application(renameBoundVariables(x, find, replace, replaceFree), renameBoundVariables(y, find, replace, replaceFree))
//
//    case Abstraction(`find`, back) => Abstraction(replace, renameBoundVariables(back, find, replace, replaceFree=true))
//    case Abstraction(front, back) if front != find && front != replace =>
//      Abstraction(front, renameBoundVariables(back, find, replace, replaceFree))
//  }




  def freeVariables(expr: Expression): List[Variable] = {
    def iter(expr: Expression, res: List[Variable], filter: List[Variable]): List[Variable] = expr match {
      case x: Variable => if(filter.contains(x)) res else res :+ x
      case Application(x, y) => res ++ iter(x, Nil, filter) ++ iter(y, Nil, filter)
      case Abstraction(front, back) => iter(back, res, filter :+ front)
    }
    iter(expr, Nil, Nil)
  }




  def boundVariables(expr: Expression): List[Variable] = {
    def iter(expr: Expression, res: List[Variable]): List[Variable] = expr match {
      case _: Variable => res
      case Application(x, y) => res ++ iter(x, Nil) ++ iter(y, Nil)
      case Abstraction(front, back) => iter(back, res :+ front)
    }
    iter(expr, Nil)
  }




  // TODO: Fix this
  def betaReduce(expr: Expression): Expression = expr match {
    case x: Variable => x
    case Application(x, y) =>
      val bv = boundVariables(x)
      substitute(x, bv.head, y)

    case Abstraction(front, back) => Abstraction(front, betaReduce(back))
  }




  def allVariables(expr: Expression): List[Variable] = freeVariables(expr) ++ boundVariables(expr)




    // Free vars are not alpha equiv
  def isAlphaEquivalent(expr1: Expression, expr2: Expression): Boolean = {
    val allVars = allVariables(expr1)

    renameAll(expr2, allVars) == expr1
  }





  // renaming all variables at the same time is ok
  def renameAll(expr: Expression, replacementVars: List[Variable]): Expression = {
    val allVars = freeVariables(expr) ++ boundVariables(expr)
    val vars = allVars.zip(replacementVars)



    def iter2(expr: Expression): Expression = expr match {
      case v: Variable =>
        vars.find(p => p._1 == v).map(_._2).get
      case Application(left, right) =>
        Application(iter2(left), iter2(right))
      case Abstraction(v, ex) =>
        Abstraction(iter2(v).asInstanceOf[Variable], iter2(ex))
    }



    iter2(expr)
  }

}


object Main extends App {
  import Tooling._

  val e1 = Abstraction(Variable('x), Variable('x))
  val e2 = Abstraction(Variable('x), Abstraction(Variable('y), Application(Variable('x), Variable('y))))
  val e3 = Application(e1, Variable('z))
  val e4 = substitute(e1, Variable('x), Variable('z))
  val e5 = substitute(e2, Variable('x), Variable('z))
  val e6 = substitute(e3, Variable('x), Variable('y))

  val e7 = Abstraction(Variable('x), Application(Variable('z), Variable('x)))

  val e8 = substitute(e7, Variable('z), e1)

  val e9 = substitute(e8, Variable('x), Variable('z))
//  val e10 = renameBoundVariables(e8, Variable('x), Variable('z))
//  val e11 = renameBoundVariables(e9, Variable('x), Variable('y))

  println(e1)
  println(e2)
  println(e3)
  println(e7)
  println(e1, " -> ", e4)
  println(e2, " -> ", e5)
  println(e3, " -> ", e6)
  println(e7, " -> ", e8)
  println(e8, " -> ", e9)
//  println(e8, " -> ", e10)
//  println(e9, " -> ", e11)
  println(freeVariables(e3))
  println(boundVariables(e8))
  println(freeVariables(Variable('x)))
}
