//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.typelevel::cats-parse:0.3.7"

package ulc

import cats.*
import cats.syntax.all.*
import cats.instances.all.*
import cats.data.NonEmptyList

type Env = Map[String, Parser.Term]

case class Location(val line: Int, val col: Int, val offset: Int)
case class Info(start: Location, end: Location):
  def merge(other: Info): Info =
    copy(end = other.end)

enum BTerm(val info: Info):
  case BVar(override val info: Info, val index: Int) extends BTerm(info)
  case BAbs(override val info: Info, val term: BTerm)             extends BTerm(info)
  case BApp(override val info: Info, val t1: BTerm, val t2: BTerm) extends BTerm(info)

  override def toString(): String =
    this match
      case BVar(_, index) => s"#$index"
      case BAbs(_, t)            => s"(λ.$t)"
      case BApp(_, t1, t2)       => s"($t1 $t2)"

object Lexer:
  import cats.parse.{ Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
  import Token.*

  enum Token(val lexeme: String, val info: Info):
    case LeftParen(override val info: Info)  extends Token("(", info)
    case RightParen(override val info: Info) extends Token(")", info)
    case Assign(override val info: Info) extends Token("=", info)
    case EndOfLine(override val info: Info) extends Token("\n", info)
    case Lambda(override val info: Info)     extends Token("\\", info)
    case Dot(override val info: Info)                                     extends Token(".", info)
    case Identifier(override val lexeme: String, override val info: Info) extends Token(lexeme, info)

  val endOfLine: P[Unit]    = R.cr | R.lf
  val whitespaces: P0[Unit] = P.until0(!R.wsp).void
  val location              = P.caret.map(c => Location(c.line, c.col, c.offset))

  val comment = (P.string("--") *> P.until0(endOfLine) *> endOfLine).void
  val comments = comment.rep0

  // token
  val leftParen  = P.char('(').info.map(p => LeftParen(p._2))
  val rightParen = P.char(')').info.map(p => RightParen(p._2))
  val assign = P.char('=').info.map(p => Assign(p._2))
  val eol = endOfLine.info.map(p => EndOfLine(p._2))
  val lambda = (P.char('\\') | P.char('λ')).info.map(p => Lambda(p._2))
  val dot    = P.char('.').info.map(p => Dot(p._2))

  val allow = R.alpha | N.digit | P.charIn('!', '@', '#', '$', '%', '^', '&', '+', '-', '*', '_', '?', '<', '>', '|')

  val identifer = allow.rep.string.info.map(p => Identifier(p._1, p._2))

  val token = comments.with1 *> (leftParen | rightParen | assign | eol | lambda | dot | identifer).surroundedBy(whitespaces) <* comments

  val parser = token.rep.map(_.toList)

  def scan(str: String): Either[String, List[Token]] =
    parser.parse(str) match
      case Right("", ls) => Right(ls)
      case Right(rest, ls) =>
        val idx = str.indexOf(rest)
        Left(s"Partial string $rest")
      case Left(err) =>
        val idx = err.failedAtOffset
        val lm  = LocationMap(str)
        Left(s"Lexer failed at $idx: $err")

  extension [T](p: P[T])
    def info: P[(T, Info)] = (location.with1 ~ p ~ location).map { case ((s, t), e) => (t, Info(s, e)) }

object Parser:
  import scala.reflect.Typeable

  import parser.Parser as P
  import parser.Parser.given
  import Lexer.Token.*
  import Lexer.Token
  import Term.*

  case class Stmt(val info: Info, val name: String, term: Term)
  enum Term(val info: Info):
    case Var(override val info: Info, val name: String)            extends Term(info)
    case Abs(override val info: Info, val arg: Var, val term: Term) extends Term(info)
    case App(override val info: Info, val t1: Term, val t2: Term) extends Term(info)

    override def toString(): String = this match
      case Var(_, name)      => name
      case Abs(_, arg, term) => s"λ$arg. $term"
      case App(_, t1, t2)       => s"($t1 $t2)"

  def test[T: Typeable](token: Token): Boolean =
    token match
      case _: T => true
      case _    => false

  def token[T: Typeable] = P.withFilter[Token](test)

  lazy val termWithoutApp: P[Token, Term] = lambda | group | variable

  lazy val variable: P[Token, Var] =
    token[Identifier].map(id => Var(id.info, id.lexeme))

  lazy val lambda: P[Token, Term] =
    for
      l  <- token[Lambda]
      id <- variable
      _  <- token[Dot]
      t  <- app
    yield Abs(l.info.merge(t.info), id, t)

  lazy val group =
    for
      _ <- token[LeftParen]
      t <- app
      _ <- token[RightParen]
    yield t

  lazy val app: P[Token, Term] = termWithoutApp.many1.map(collapse)

  lazy val stmt =
    for
      v <- variable
      _ <- token[Assign]
      t <- app
    yield Stmt(v.info.merge(t.info), v.name, t)

  lazy val line: P[Token, Stmt | Term] = stmt | app

  lazy val program: P[Token, NonEmptyList[Stmt | Term]] = (line <* token[EndOfLine].?).many1

  def collapse(ts: NonEmptyList[Term]): Term =
    ts match
      case NonEmptyList(head, Nil) => head
      case NonEmptyList(head, x::Nil) => App(head.info.merge(x.info), head, x)
      case NonEmptyList(head, x::y::xs) => App(mergeInfo(ts), App(head.info.merge(x.info), head, x), collapse(NonEmptyList(y, xs)))

  def mergeInfo(ts: NonEmptyList[Term]): Info =
    ts.foldLeft(ts.head.info)((a, b) => a.merge(b.info))

  def parse[B](p: P[Token, B])(tokens: List[Token]): Either[String, B] =
    p.parse(tokens) match
      case Left(err)          => Left(err)
      case Right((Nil, term)) => Right(term)
      case _                  => Left("Partial Parse")

object DeBruijn:
  import BTerm.*
  import Parser.Term.*
  import Parser.Term
  import scala.collection.mutable.ListBuffer

  def transform(env: Env, free: List[String]): Term => BTerm =
    // TODO mutation doesn't work
    val mFree = ListBuffer.from(free)
    def go(ctx: List[String]): Term => BTerm =
      case Var(fi, x) =>
        val idx =  ctx.indexOf(x)
        if idx == -1 then
          env.get(x) match
          case Some(term) => transform(env, mFree.toList)(term)
          case None =>
            val freeIdx = mFree.indexOf(x)
            if freeIdx == -1 then
              mFree += x
              BVar(fi, free.length -1)
            else BVar(fi, freeIdx)
        else
          BVar(fi, idx)
      case Abs(fi, x, t) =>
        BAbs(fi, go(x.name::ctx)(t))
      case App(fi, t1, t2) =>
        BApp(fi, go(ctx)(t1), go(ctx)(t2))
    go(Nil)

object Evaluation:
  import BTerm as Term
  import BTerm.*

  def termShift(d: Int): Term => Term = termShift(d, 0)

  // the shift operator ↑ d c for term t
  def termShift(d: Int, c: Int): Term => Term =
    def onVar(v: BVar, c: Int): BVar =
      if v.index >= c then
        v.copy(index = v.index + d)
      else
        v
    map(onVar, c)

  // The substitution [j ↦ s] t of term s for numbered j in term t
  def termSubst(j: Int, s: Term): Term => Term =
    def onVar(v: BVar, c: Int): Term =
      if v.index == j + c then termShift(c)(s)
      else v
    map(onVar, 0)

  def termSubstTop(s: Term): Term => Term =
    (termSubst(0, termShift(1)(s))) andThen termShift(-1)

  def map(onVar: (BVar, Int) => Term, c: Int)(term: Term): Term =
    def walk(onVar: (BVar, Int) => Term, c: Int, t: Term): Term =
      t match
        case t @ BVar(_, _) => onVar(t, c)
        case BAbs(fi, t1)      => BAbs(fi, walk(onVar, c + 1, t1))
        case BApp(fi, t1, t2)  => BApp(fi, walk(onVar, c, t1), walk(onVar, c, t2))
    walk(onVar, c, term)

  def isVal: Term => Boolean =
    case BAbs(_, _) => true
    case _           => false

  def eval1(env: Env, term: Term): (Term, Boolean) =
    term match
      case BApp(fi, BAbs(_, t12), v2) if isVal(v2) =>
        (termSubstTop(v2)(t12), false)
      case BApp(fi, t1, t2) if isVal(t1) =>
        val r1 = eval1(env, t2)
        (BApp(fi, t1, r1._1), r1._2)
      case BApp(fi, t1, t2) =>
        val r1 = eval1(env, t1)
        (BApp(fi, r1._1, t2), r1._2)
      case _ => (term, true)

  def eval(env: Env, term: Term): Term =
    val t1 = eval1(env, term)
    t1 match
      case (t, false) => eval(env, t)
      case (t, true)  => t

  def eval(term: Term): Term =
    eval(Map.empty, term)

class Interpreter:
  import Parser.{Term, Stmt}
  import scala.collection.mutable.Map

  val env = Map[String, Term]()

  def load(program: String): Either[String, Unit] =
    for
      ts <- Lexer.scan(program)
      p <- Parser.parse(Parser.program)(ts)
      _ = p.map(eval)
    yield ()

  def eval(input: String): String =
    val r = for
      ts <- Lexer.scan(input)
      t <- Parser.parse(Parser.line)(ts)
    yield t
    r match
      case Left(str) =>
        s"Parse Error: $str"
      case Right(line) =>
        eval(line)

  def eval(line: Term | Stmt): String =
    line match
      case t: Term =>
        val bTerm = DeBruijn.transform(env.toMap, Nil)(t)
        Evaluation.eval(env.toMap, bTerm).toString
      case Stmt(_, name, term) =>
        env += (name -> term)
        s"$name = $term"
