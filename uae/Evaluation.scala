//> using scala "3.2.0"

// case class Location(val line: Int, val col: Int, val offset: Int)
// case class Info(start: Location, end: Location)

package uae
type Info = Unit

enum Term(val info: Info):
  case TTrue(override val info: Info)                                                      extends Term(info)
  case TFalse(override val info: Info)                                                     extends Term(info)
  case TIf(override val info: Info, val condition: Term, val tTerm: Term, val fTerm: Term) extends Term(info)
  case TZero(override val info: Info)                                                      extends Term(info)
  case TSucc(override val info: Info, val term: Term)                                      extends Term(info)
  case TPrev(override val info: Info, val term: Term)                                      extends Term(info)
  case TIsZero(override val info: Info, val term: Term)                                    extends Term(info)

  def isNumericVal: Boolean = this match
    case TZero(_)     => true
    case TSucc(_, t1) => t1.isNumericVal
    case _            => false

  def isBoolVal = this match
    case TTrue(_)  => true
    case TFalse(_) => true
    case _         => false

  def isVal = isBoolVal || isNumericVal

object Evaluation:
  import Term.*
  def eval1(term: Term): (Term, Option[Term]) = term match
    case TIf(_, TTrue(_), t2, _)                     => (t2, None)
    case TIf(_, TFalse(_), _, t3)                    => (t3, None)
    case t @ TIf(_, t1, t2, t3)                      => (t.copy(condition = eval1(t1)._1), None)
    case t @ TSucc(_, t1) if t1.isNumericVal         => (t, Some(t))
    case t @ TSucc(_, t1)                            => (t.copy(term = eval1(t1)._1), None)
    case TPrev(fi, t @ TZero(_))                     => (t, Some(t))
    case TPrev(_, TSucc(_, nv1)) if nv1.isNumericVal => (nv1, Some(nv1))
    case t @ TPrev(_, t1)                            => (t.copy(term = eval1(t1)._1), None)
    case TIsZero(fi, TZero(_))                       => (TTrue(fi), None)
    case t @ TIsZero(_, t1)                          => (t.copy(term = eval1(t1)._1), None)
    case t @ TTrue(_)                                => (t, Some(t))
    case t @ TFalse(_)                               => (t, Some(t))
    case t @ TZero(_)                                => (t, Some(t))
    case _                                           => throw RuntimeException("NoRuleApplies")

  def eval(term: Term): Term = eval1(term) match
    case (_, Some(t)) => t
    case (t, None)    => eval(t)

@main
def main =
  import Term.*
  import Evaluation.*
  val t1 = TIf((), TTrue(()), TTrue(()), TTrue(()))
  val t2 = TIf((), TTrue(()), TSucc((), TSucc((), TZero(()))), TTrue(()))
  val t3 = TIf((), TTrue(()), TSucc((), TSucc((), TPrev((), TSucc((), TZero(()))))), TTrue(()))
  println(eval(t1))
  println(eval(t2))
  println(eval(t3))
