package kdkocev.hilbert

import org.specs2.mutable.Specification

class HilbertSystemSpecs extends Specification {
  import Main._
  "HilbertSystem" should {
    "prove" in {
      "Hypothesis" in {
        prove(List(Hypothesis(F('A))), F('A)) mustEqual true
      }
      "Axiom" in {
        prove(List(Axiom1(F('A), F('B))), Implication(F('A), Implication(F('B), F('A)))) mustEqual true
      }
      "(not(D) -> C)) can be derived from {A, (A->B), (B->C)}" in {
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
      prove(test, Implication(Not(F('D)), F('C))) mustEqual true
      }
    }
  }
}
