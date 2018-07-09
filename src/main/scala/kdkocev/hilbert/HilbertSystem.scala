package kdkocev.hilbert

import scala.annotation.tailrec

trait Formula

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

case class Hypothesis(A: Formula) extends Formula
case class ModusPonens(formulaLine1: Int, formulaLine2: Int) extends Formula


object Main extends App {

  def prove(proof: List[Formula], required: Formula): Boolean = {
    // Stop and return a feedback
    def stop(couldNotProve: Formula, proven: Map[Int, Formula], hypothesis: Map[Int, Formula]): Boolean = {
      println("could not prove", couldNotProve)
      println("proven", proven)
      println("hypothesis", hypothesis)
      false
    }

    // tailrecursive function that does the calculation by matching over the list of formulas
    @tailrec
    def iter(formulas: List[Formula], proven: Map[Int, Formula], hypothesis: Map[Int, Formula], iteration: Int): Boolean = {
      formulas match {
        // If there are no more formulas to apply -> see if the required formula has been proven
        case Nil =>
          proven.contains{
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

        // If the current formula is an axiom - add it to the proven Map
        case (a1: Axiom1) :: tail => iter(tail, proven + (iteration -> a1.toFormula), hypothesis, iteration+1)
        case (a2: Axiom2) :: tail => iter(tail, proven + (iteration -> a2.toFormula), hypothesis, iteration+1)
        case (a3: Axiom3) :: tail => iter(tail, proven + (iteration -> a3.toFormula), hypothesis, iteration+1)

        // If the current formula is a hypothesis - add it to hypothesis
        case Hypothesis(a) :: tail =>
          iter(tail, proven, hypothesis + (iteration -> a), iteration+1)

        // if the current formula is modus ponens
        case ModusPonens(index1, index2) :: tail =>
          // Check if the left side is something that we can find either in proven or hypothesis
          val leftSide = if(proven.contains(index1))
            proven(index1)
          else if(hypothesis.contains(index1))
            hypothesis(index1)
          else {
            stop(ModusPonens(index1, index2), proven, hypothesis)
          }


          // If the right side is a hypothesis, check if it is an Implication
          if (hypothesis.contains(index2)) {
            hypothesis(index2) match {
              case Implication(`leftSide`, y) =>
                iter(tail, proven + (iteration -> y), hypothesis, iteration + 1)

              case _ => stop(ModusPonens(index1, index2), proven, hypothesis)
            }


          // If the right side is a proof - check if it is an axiom or an implication
          } else if (proven.contains(index2)) {
            proven(index2) match {
              // If it is the first axiom - check if the left side is what it should be
              case a1: Axiom1 =>
                if (a1.A == leftSide) {
                  val res = Implication(a1.B, a1.A)
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }

              // Check the same for the second axiom
              case a2: Axiom2 =>
                if (Implication(a2.A, Implication(a2.B, a2.C)) == leftSide) {
                  val res = Implication(Implication(a2.A, a2.B), Implication(a2.A, a2.C))
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }

              // Same for the third one
              case a3: Axiom3 =>
                if (Implication(Not(a3.A), Not(a3.B)) == leftSide) {
                  val res = Implication(a3.B, a3.A)
                  iter(tail, proven + (iteration -> res), hypothesis, iteration + 1)
                } else {
                  stop(ModusPonens(index1, index2), proven, hypothesis)
                }

              // If it is an implication - check if the left side is equal to `leftSide` and
              // add the right side to "proven" formulas
              case Implication(`leftSide`, b) =>
                iter(tail, proven + (iteration -> b), hypothesis, iteration+1)
            }
          } else {
            println("proven and hypothesis do not contain " + index1)
            stop(ModusPonens(index1, index2), proven, hypothesis)
          }
      }
    }


    iter(proof, Map(), Map(), 1)
  }

  // Example proof that (not(D) -> C)) is correct in {A, (A->B), (B->C)}
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