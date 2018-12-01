package kdkocev.typedlambdacalculus.expressionModelPrototype

trait Type

// placeholder type
case object Nothing extends Type

// t1 -> t2
case class FuncType(t1: Type, t2: Type) extends Type
