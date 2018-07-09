package kdkocev.hilbert

trait Formula {
//  def customToString: String = this match {
//    case F(x) => x.toString
//    case Not(x) => s"¬$x"
//    case Implication(x, y) => s"$x → $y"
//    case Axiom1(x, y) => s"($x → ($y → $x))"
//    case Axiom2(x, y, z) => s"(($x → ($y → $z)) → (($x → $y) → ($x → $z)))"
//    case Axiom3(x, y) => s"(((¬$x) → (¬$y)) → ($y → $x))"
//    case ModusPonens(_, Hypothesis(Implication(_, y))) => y.toString
//    case ModusPonens(_, Axiom1(_, y)) => y.toString
//    case Hypothesis(x) => x.toString
//  }
}

case class F(A: Symbol) extends Formula
case class Not(A: Formula) extends Formula
case class Implication(A: Formula, B: Formula) extends Formula

// Axioms
case class Axiom1(A: Formula, B: Formula) extends Formula {
  def toFormula = Implication(A, Implication(B, A))
}
case class Axiom2(A: Formula, B: Formula, C: Formula) extends Formula {
  def toFormula = Implication(
    Implication(A, Implication(B, C)),
    Implication(
      Implication(A, B),
      Implication(A, C)
    )
  )
}
case class Axiom3(A: Formula, B: Formula) extends Formula {
  def toFormula = Implication(
    Implication(Not(A), Not(B)),
    Implication(B, A)
  )
}

// Hypothesis
case class Hypothesis(A: Formula) extends Formula

// Modus ponens
case class ModusPonens(formulaLine1: Int, formulaLine2: Int) extends Formula

// Proof system Г(S, R)
// S = finite set of axiom scemata
// R = finite set of proof rules

object Main extends App {
  def prove(proof: List[Formula], required: Formula): Boolean = {
    def stop(couldNotProve: Formula, proven: Map[Int, Formula], hypothesis: Map[Int, Formula]): Boolean = {
      println("could not prove", couldNotProve)
      println("proven", proven)
      println("hypothesis", hypothesis)
      false
    }

    def iter(formulas: List[Formula], proven: Map[Int, Formula], hypothesis: Map[Int, Formula], iteration: Int): Boolean = {
      formulas match {
        case Nil =>
          proven.exists {
            case (key, `required`) =>
              println("proven", proven)
              println("hypothesis", hypothesis)
              true
            case _ =>
              println("required formula not proven")
              println("proven", proven)
              println("hypothesis", hypothesis)
              false
          }

        case (a1: Axiom1) :: tail => iter(tail, proven + (iteration -> a1.toFormula), hypothesis, iteration+1)
        case (a2: Axiom2) :: tail => iter(tail, proven + (iteration -> a2.toFormula), hypothesis, iteration+1)
        case (a3: Axiom3) :: tail => iter(tail, proven + (iteration -> a3.toFormula), hypothesis, iteration+1)

        case ModusPonens(index1, index2) :: tail =>
          val leftSide = if(proven.contains(index1))
            proven(index1)
          else
            hypothesis(index1)


          if (hypothesis.contains(index2)) {
            hypothesis(index2) match {
              case Implication(`leftSide`, y) =>
                iter(tail, proven + (iteration -> y), hypothesis, iteration + 1)
              case _ => stop(ModusPonens(index1, index2), proven, hypothesis)
            }


          } else if (proven.contains(index2)) {
            proven(index2) match {
              case a1: Axiom1 =>
                if (a1.A == leftSide) {
                  val res = Implication(a1.B, a1.A)
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }
              case a2: Axiom2 =>
                if (Implication(a2.A, Implication(a2.B, a2.C)) == leftSide) {
                  val res = Implication(Implication(a2.A, a2.B), Implication(a2.A, a2.C))
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }
              case a3: Axiom3 =>
                if (Implication(Not(a3.A), Not(a3.B)) == leftSide) {
                  val res = Implication(a3.B, a3.A)
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }
              case Implication(`leftSide`, b) =>
                iter(tail, proven + (iteration -> b), hypothesis, iteration+1)
            }
          } else {
            println("proven and hypothesis do not contain " + index1)
            stop(ModusPonens(index1, index2), proven, hypothesis)
          }
        case Hypothesis(a) :: tail =>
          iter(tail, proven, hypothesis + (iteration -> a), iteration+1)
      }
    }


    iter(proof, Map(), Map(), 1)
  }

  val step1 = Hypothesis(F('A))
  val step2 = Hypothesis(Implication(F('A), F('B)))
  val step3 = ModusPonens(1, 2)
  val step4 = Hypothesis(Implication(F('B), F('C)))
  val step5 = ModusPonens(3, 4)
  val step6 = Axiom1(F('C), Not(F('D)))
  val step7 = ModusPonens(5, 6)

  val test = List(
    step1, step2, step3, step4, step5, step6, step7
  )

  val res = prove(test, Implication(Not(F('D)), F('A)))

  println(res)
}