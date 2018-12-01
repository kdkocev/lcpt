package kdkocev.typedlambdacalculus.expressionModel

sealed trait Expression
sealed trait UExpression extends Expression
sealed trait TExpression extends Expression {val t: Type}

sealed trait Variable {val s: Symbol}
final case class UVar(override val s: Symbol) extends Variable with UExpression
final case class TVar(override val s: Symbol, override val t: Type) extends Variable with TExpression

sealed trait Application {val u: Expression; val v: Expression}
final case class UApp(override val u: Expression, override val v: Expression) extends Application with UExpression
final case class TApp(override val u: TExpression, override val v: TExpression, override val t: Type) extends Application with TExpression

sealed trait Abstraction {val arg: Variable; val body: Expression}
final case class UAbs(override val arg: Variable, override val body: Expression) extends Abstraction with UExpression
final case class TAbs(override val arg: TVar, override val body: TExpression, override val  t: Type) extends Abstraction with TExpression

case class Constant(s: Symbol, override val t: Type) extends TExpression
