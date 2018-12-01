package kdkocev.typedlambdacalculus.expressionModel

trait Type

// placeholder type
case object Nothing extends Type

// t1 -> t2
case class FuncType(t1: Type, t2: Type) extends Type

case object TInt extends Type
case object TString extends Type