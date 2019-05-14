package kdkocev.simplytypedlabmdacalculus

trait Type
case class To[T1 <: Type, T2 <: Type](t1: T1, t2: T2) extends Type {
  override def toString: String = s"($t1 -> $t2)"
}

trait Expression {
  def typ: Type = this match {
    case v: Variable => v.t
    case Abstraction(v, b) => To(v.typ, b.typ)
    case Application(e1, e2) => {
      (e1.typ, e2.typ) match {
        case (To(t1, t2), t3) if t1 == t3 => t2
        case _ => throw new Error(s"Tried to apply $e2 to $e1 but types dont match")
      }
    }
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

  val abs1 = Abstraction(x, x)
//  println(abs1.typ) // (σ -> σ)

  val app1 = Application(abs1, y)
//  println(app1.typ) // σ

  val abs2 = Abstraction(Variable('x, To(Sigma, Sigma)), Application(Variable('x, To(Sigma, Sigma)), y))
  println(abs2.typ)

  
}