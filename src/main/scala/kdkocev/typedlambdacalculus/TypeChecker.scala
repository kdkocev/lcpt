package kdkocev.typedlambdacalculus

import expressionModel._

object TypeChecker {
  def apply(e: TExpression): Either[Error, TExpression] = e match {
    case Constant(s, t) => Right(e)
    case TVar(s, t) => Right(e)
    case TApp(u, v, t) =>
      TypeChecker(u).flatMap(
        uc => TypeChecker(v).flatMap(
          vc =>
            applyTypes(uc.t, vc.t).flatMap(appliedType =>
              if(appliedType == t) {
                Right(e)
              } else {
                Left(new Error("Mismatch"))
              }
            )
        )
      )
    case TAbs(arg, body, t) =>
      TypeChecker(arg).flatMap(
        argC => TypeChecker(body).flatMap(bodyC =>
          if(composeTypes(argC.t, bodyC.t) == t) {
            Right(e)
          } else {
            Left(new Error("Type Mismatch"))
          }
        )
      )
    case _ => Left(new Error("Unknown expression"))
  }

  // (t1 -> t2) (t1) => t2
  def applyTypes(t1: Type, t2: Type): Either[Error, Type] = t1 match {
    case FuncType(`t2`, typ2) => Right(typ2)
    case _ => Left(new Error("Type application not possible"))
  }

  // creates t1 -> t2
  def composeTypes(t1: Type, t2: Type): Type = FuncType(t1, t2)
}

object Main extends App {
  val e1 = TAbs(TVar('x, TInt), TVar('y, TString), FuncType(TInt, TString))

  println(TypeChecker(e1))

  val e2 = TApp(e1, Constant('z, TInt), TString)

  println(TypeChecker(e2))
}