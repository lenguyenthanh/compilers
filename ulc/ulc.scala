//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.typelevel::cats-parse:0.3.7"

package ulc

import cats.instances.all.*
import cats.syntax.all.*
import cats.data.NonEmptyList

case class Location(val line: Int, val col: Int, val offset: Int)
case class Info(start: Location, end: Location):
  def merge(other: Info): Info =
    this.copy(end = other.end)

enum Token(val lexeme: String, val info: Info):
  case LeftParen(override val info: Info)  extends Token("(", info)
  case RightParen(override val info: Info) extends Token(")", info)
  case Lambda(override val info: Info)     extends Token("\\", info)
  // case Assign(override val info: Info) extends Token("=", info)
  case Dot(override val info: Info)                                     extends Token(".", info)
  case Identifier(override val lexeme: String, override val info: Info) extends Token(lexeme, info)

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

object Lexer:
  import cats.parse.{ Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
  import Token.*
  val endOfLine: P[Unit]    = R.cr | R.lf
  val whitespaces: P0[Unit] = P.until0(!R.wsp).void
  val location              = P.caret.map(c => Location(c.line, c.col, c.offset))

  // token
  val leftParen  = P.char('(').info.map(p => LeftParen(p._2))
  val rightParen = P.char(')').info.map(p => RightParen(p._2))
  // val assign = P.char('=').info.map(p => Assign(p._2))
  val lambda = (P.char('\\') | P.char('λ')).info.map(p => Lambda(p._2))
  val dot    = P.char('.').info.map(p => Dot(p._2))

  val allow = R.alpha | N.digit | P.charIn('!', '@', '#', '$', '%', '^', '&', '*', '_', '?', '<', '>')

  val identifer = allow.rep.string.info.map(p => Identifier(p._1, p._2))

  val token = (leftParen | rightParen | lambda | dot | identifer).surroundedBy(whitespaces) // | assign

  val parser = token.rep.map(_.toList)

  def scan(str: String): Either[String, List[Token]] =
    parser.parse(str) match
      case Right("", ls) => Right(ls)
      case Right(rest, ls) =>
        val idx = str.indexOf(rest)
        Left(s"Parital string $rest")
      case Left(err) =>
        val idx = err.failedAtOffset
        val lm  = LocationMap(str)
        Left(s"Lexer failed at $idx: $err")

  extension [T](p: P[T])
    def info: P[(T, Info)] = (location.with1 ~ p ~ location).map { case ((s, t), e) => (t, Info(s, e)) }

object Parser:
  import scala.reflect.Typeable

  import Token.*
  import Term.*
  import parser.Parser as P
  import parser.Parser.given

  enum Term(val info: Info):
    case Var(override val info: Info, val name: String)            extends Term(info)
    case Abs(override val info: Info, val arg: Var, val term: App) extends Term(info)
    case App(override val info: Info, val ts: List[Term])          extends Term(info)
    case Group(override val info: Info, val term: Term)            extends Term(info)

    override def toString(): String = this match
      case Var(_, name)      => name
      case Abs(_, arg, term) => s"λ$arg. $term"
      case App(_, ts)        => ts.mkString(" ")
      case Group(_, t)       => s"($t)"

  def test[T: Typeable](token: Token): Boolean =
    token match
      case _: T => true
      case _    => false

  def token[T: Typeable]: P[Token, T] = new P[Token, T] {
    def parse(input: List[Token]): Either[String, (List[Token], T)] =
      // println(s"token ${input.length}")
      input match
        case head :: tail =>
          head match
            case t: T => Right(tail, t)
            case _    => Left("unexpected token")
        case _ => Left("Empty input")
  }

  def term: P[Token, Term]           = lambda | group | app | variable
  def termWithoutApp: P[Token, Term] = lambda | group | variable
  lazy val variable: P[Token, Var] =
    token[Identifier].map(id => Var(id.info, id.lexeme))

  lazy val lambda: P[Token, Term] =
    for
      l <- token[Lambda]
      // _ = println(l)
      id <- variable
      // _ = println(id)
      _ <- token[Dot]
      t <- app
    // _ = println(t)
    yield Abs(l.info.merge(t.info), id, t)

  lazy val app: P[Token, App] = termWithoutApp.many1.map(ts => App(mergeInfo(ts), ts.toList))

  def mergeInfo(ts: NonEmptyList[Term]): Info =
    ts.foldLeft(ts.head.info)((a, b) => a.merge(b.info))

  lazy val group =
    for
      s <- token[LeftParen]
      t <- term
      e <- token[RightParen]
    yield Group(s.info.merge(e.info), t)

  def parse(tokens: List[Token]): Either[String, Term] =
    term.parse(tokens) match
      case Left(err)          => Left(err)
      case Right((Nil, term)) => Right(term)
      case _                  => Left("Partial Parse")

object Evaluation:
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
      if v.index == j + c then termShift(c)(s)
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
