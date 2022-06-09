//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.typelevel::cats-parse:0.3.7"

// case class Location(val line: Int, val col: Int, val offset: Int)
// case class Info(start: Location, end: Location)
type Info = Unit

type NameBinding = Unit
type Context     = List[(String, NameBinding)]

enum Term(val info: Info):
  case TMVar(override val info: Info, val index: Int, total: Int) extends Term(info)
  case TMAbs(override val info: Info, val term: Term)             extends Term(info)
  case TMApp(override val info: Info, val t1: Term, val t2: Term) extends Term(info)

  override def toString(): String =
    this match
      case TMVar(_, index, total) => s"#$index"
      case TMAbs(_, t)            => s"λ.$t"
      case TMApp(_, t1, t2)       => s"($t1 $t2)"

object Term:
  import Term.*

  def termShift(d: Int): Term => Term = termShift(d, 0)

  // the shift operator ↑ d c for term t
  def termShift(d: Int, c: Int): Term => Term =
    def onVar(v: TMVar, c: Int): TMVar =
      if v.index >= c then v.copy(index = v.index + d, total = v.total + d)
      else v.copy(total = v.total + d)
    map(onVar, c)

  // The substitution [j ↦ s] t of term s for numbered j in term t
  def termSubst(j: Int, s: Term): Term => Term =
    def onVar(v: TMVar, c: Int): Term =
      if v.index == j + c then
        termShift(c)(s)
      else v
    map(onVar, 0)

  def termSubstTop(s: Term): Term => Term =
    (termSubst(0, termShift(1)(s))) andThen termShift(-1)

  def map(onVar: (TMVar, Int) => Term, c: Int)(term: Term): Term =
    def walk(onVar: (TMVar, Int) => Term, c: Int, t: Term): Term =
      t match
        case t @ TMVar(_, _, _) => onVar(t, c)
        case TMAbs(fi, t1)      => TMAbs(fi, walk(onVar, c + 1, t1))
        case TMApp(fi, t1, t2)  => TMApp(fi, walk(onVar, c, t1), walk(onVar, c, t2))
    walk(onVar, c, term)

  def isVal: Term => Boolean =
    case TMAbs(_, _) => true
    case _           => false

object Evaluation:
  import Term.*

  def eval1(term: Term): (Term, Boolean) =
    term match
      case TMApp(fi, TMAbs(_, t12), v2) if isVal(v2) =>
        (termSubstTop(v2)(t12), false)
      case TMApp(fi, v1, t2) if isVal(v1) =>
        (TMApp(fi, v1, eval1(t2)._1), false)
      case TMApp(fi, t1, v2) if isVal(v2) =>
        (TMApp(fi, eval1(t1)._1, v2), false)
      case _ => (term, true)

  def eval(term: Term): Term =
    val t1 = eval1(term)
    t1 match
      case (t, false) => eval(t)
      case (t, true)  => t

@main
def main =
  import Term.*
  import Evaluation.*
  // (λ.1 0 2) (λ.0) -> 0 (λ.0) 1
  val t = TMApp(
    (),
    TMAbs((), TMApp((), TMApp((), TMVar((), 1, 3), TMVar((), 0, 3)), TMVar((), 2, 3))),
    TMAbs((), TMVar((), 0, 1))
  )
  println(t)
  println(eval(t))
