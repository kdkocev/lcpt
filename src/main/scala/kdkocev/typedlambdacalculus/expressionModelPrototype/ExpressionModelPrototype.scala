package kdkocev.typedlambdacalculus.expressionModelPrototype

trait Expression
trait UExpression extends Expression
trait TExpression extends Expression

trait Variable
case class UVar(s: Symbol) extends Variable with UExpression
case class TVar(s: Symbol, t: Type) extends Variable with TExpression

trait Application
case class UApp(u: Expression, v: Expression) extends Application with UExpression
case class TApp(u: TExpression, v: TExpression, t: Type) extends Application with TExpression

trait Abstraction
case class UAbs(arg: Variable, body: Expression) extends Abstraction with UExpression
case class TAbs(arg: TVar, body: TExpression, t: Type) extends Abstraction with TExpression

case class Constant(s: Symbol, t: Type) extends TExpression

object Main extends App {

  val v: TExpression = TVar('s, Nothing)
  val ap: TExpression = TApp(v, v, Nothing)
  val ab: TExpression = TAbs(TVar('s, Nothing), ap, Nothing)

  println("success")
}