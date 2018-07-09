package kdkocev.hilbert

trait Formula {
  override def toString: String = this match {
    case F(x) => x.toString
    case Not(x) => s"¬$x"
    case Implication(x, y) => s"$x → $y"
    case Axiom1(x, y) => s"($x → ($y → $x))"
    case Axiom2(x, y, z) => s"(($x → ($y → $z)) → (($x → $y) → ($x → $z)))"
    case Axiom3(x, y) => s"(((¬$x) → (¬$y)) → ($y → $x))"
    case ModusPonens(_, Hypotesis(Implication(_, y))) => y.toString
    case ModusPonens(_, Axiom1(_, y)) => y.toString
    case Hypotesis(x) => x.toString
  }
}

case class F(A: Symbol) extends Formula
case class Not(A: Formula) extends Formula
case class Implication(A: Formula, B: Formula) extends Formula

// Axioms
case class Axiom1(A: Formula, B: Formula) extends Formula
case class Axiom2(A: Formula, B: Formula, C: Formula) extends Formula
case class Axiom3(A: Formula, B: Formula) extends Formula

// Hypotesis
case class Hypotesis(A: Formula) extends Formula

// Modus ponens
// takes an implication as a second argument
case class ModusPonens(A: Formula, B: Formula) extends Formula

// Proof system Г(S, R)
// S = finite set of axiom scemata
// R = finite set of proof rules

object Main extends App {
//  val proof = ???

  val step1 = Hypotesis(F('A))
  val step2 = Hypotesis(Implication(F('A), F('B)))
  val step3 = ModusPonens(step1, step2)
  val step4 = Hypotesis(Implication(F('B), F('C)))
  val step5 = ModusPonens(step3, step4)
  val step6 = Axiom1(F('C), Not(F('D)))
  val step7 = ModusPonens(step5, step6)

  val p =
    ModusPonens(
      ModusPonens(
        ModusPonens(
          Hypotesis(
            F('A)
          ),
          Hypotesis(
            Implication(F('A), F('B))
          )
        ),
        Hypotesis(Implication(F('B), F('C)))
      ),
      Axiom1(F('C), F('D))
    )

  println(p)

}