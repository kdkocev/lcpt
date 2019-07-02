package kdkocev.hilbert2

trait Formula

case class F(A: Symbol) extends Formula
case class Not(A: Formula) extends Formula
case class Implication(A: Formula, B: Formula) extends Formula

case class Axiom1(A: Formula, B: Formula) extends Formula {
  def toFormula: Formula = Implication(A, Implication(B, A))
}

case class Axiom2(A: Formula, B: Formula, C: Formula) extends Formula {
  def toFormula: Formula = Implication(
    Implication(A, Implication(B, C)),
    Implication(
      Implication(A, B),
      Implication(A, C)
    )
  )
}

case class Axiom3(A: Formula, B: Formula) extends Formula {
  def toFormula: Formula = Implication(
    Implication(Not(A), Not(B)),
    Implication(B, A)
  )
}

case class ModusPonens(f1: Formula, f2: Formula) extends Formula

object Main extends App {
  def checkProof(proof: List[Formula], required: Formula): Boolean = {

    def iter(proof: List[Formula], proven: List[Formula]): Boolean = {
      proof match {
        case Nil if proven.contains(required) => true
        case Axiom1(a, b) :: tail if proven.contains(a) && proven.contains(b) =>
          iter(tail, proven :+ Axiom1(a, b))
        case Axiom2(a, b, c) :: tail if proven.contains(a) && proven.contains(b) && proven.contains(c) =>
          iter(tail, proven :+ Axiom2(a, b, c))
        case Axiom3(a, b) :: tail if proven.contains(a) && proven.contains(b) =>
          iter(tail, proven :+ Axiom3(a, b))
        case ModusPonens(f1, f2) :: tail if proven.contains(f1) =>
          iter(tail, proven :+ f2)
        case F(x) :: tail => iter(tail, proven :+ F(x))
        case e =>
          println(s"Match error. Could not match $e")
          false
      }
    }

    iter(proof, Nil)
  }

  val step1 = F('A)
  val step2 = F('B)
  val step3 = Axiom1(F('A), Implication(F('B), F('A)))
  val step4 = Axiom2(F('A), Implication(F('B), F('A)), F('A))
  val step5 = ModusPonens(
    step3,
    step4
  )
  val step6 = Axiom1(F('A), F('B))
  val step7 = ModusPonens(step5, step6)

  val proof = List(
    step1,
    step2,
    step3,
    step4,
    step5,
    step6,
    step7
  )

  checkProof(proof, Implication(F('A), F('A)))
}