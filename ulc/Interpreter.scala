//> using scala "3.2.2"
//> using lib "org.typelevel::cats-core::2.8.0"
//> using lib "org.typelevel::cats-parse::0.3.9"

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

enum BTerm:
  case BVar(val index: Int)
  case BAbs(val term: BTerm)
  case BApp(val t1: BTerm, val t2: BTerm)

  override def toString(): String =
    this match
      case BVar(index)  => s"#$index"
      case BAbs(t)      => s"(λ.$t)"
      case BApp(t1, t2) => s"($t1 $t2)"

object Lexer:
  import cats.parse.{ Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
  import Token.*

  enum Token(val lexeme: String, val info: Info):
    case LeftParen(override val info: Info)                               extends Token("(", info)
    case RightParen(override val info: Info)                              extends Token(")", info)
    case Assign(override val info: Info)                                  extends Token("=", info)
    case EndOfLine(override val info: Info)                               extends Token("\n", info)
    case Lambda(override val info: Info)                                  extends Token("\\", info)
    case Dot(override val info: Info)                                     extends Token(".", info)
    case Identifier(override val lexeme: String, override val info: Info) extends Token(lexeme, info)

  val endOfLine: P[Unit]    = R.cr | R.lf
  val whitespaces: P0[Unit] = P.until0(!R.wsp).void
  val location              = P.caret.map(c => Location(c.line, c.col, c.offset))

  val comment  = (P.string("--") *> P.until0(endOfLine) *> endOfLine).void
  val comments = comment.rep0

  // token
  val leftParen  = P.char('(').info.map(p => LeftParen(p._2))
  val rightParen = P.char(')').info.map(p => RightParen(p._2))
  val assign     = P.char('=').info.map(p => Assign(p._2))
  val eol        = endOfLine.info.map(p => EndOfLine(p._2))
  val lambda     = (P.char('\\') | P.char('λ')).info.map(p => Lambda(p._2))
  val dot        = P.char('.').info.map(p => Dot(p._2))

  val allow =
    R.alpha | N.digit | P.charIn('!', '@', '#', '$', '%', '^', '&', '+', '-', '*', '_', '?', '<', '>', '|', '\'')

  val identifer = allow.rep.string.info.map(p => Identifier(p._1, p._2))

  val token = comments.with1 *> (leftParen | rightParen | assign | eol | lambda | dot | identifer)
    .surroundedBy(whitespaces) <* comments

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
    case Var(override val info: Info, val name: String)             extends Term(info)
    case Abs(override val info: Info, val arg: Var, val term: Term) extends Term(info)
    case App(override val info: Info, val t1: Term, val t2: Term)   extends Term(info)

    override def toString(): String = this match
      case Var(_, name)      => name
      case Abs(_, arg, term) => s"λ$arg. $term"
      case App(_, t1, t2)    => s"($t1 $t2)"

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
      case NonEmptyList(head, Nil)      => head
      case NonEmptyList(head, x :: Nil) => App(head.info.merge(x.info), head, x)
      case NonEmptyList(head, x :: y :: xs) =>
        App(mergeInfo(ts), App(head.info.merge(x.info), head, x), collapse(NonEmptyList(y, xs)))

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
    val localFree = ListBuffer.from(free)
    def go(ctx: List[String]): Term => BTerm =
      case Var(fi, x) =>
        val idx = ctx.indexOf(x)
        if idx == -1 then
          env.get(x) match
            case Some(term) => transform(env, localFree.toList)(term)
            case None =>
              val freeIdx = localFree.indexOf(x)
              if freeIdx == -1 then
                localFree += x
                BVar(free.length - 1)
              else BVar(freeIdx)
        else BVar(idx)
      case Abs(fi, x, t) =>
        BAbs(go(x.name :: ctx)(t))
      case App(fi, t1, t2) =>
        BApp(go(ctx)(t1), go(ctx)(t2))
    go(Nil)

object Evaluation:
  import BTerm.*

  def termShift(d: Int): BTerm => BTerm = termShift(d, 0)

  // the shift operator ↑ d c for term t
  def termShift(d: Int, c: Int): BTerm => BTerm =
    def onVar(v: BVar, c: Int): BVar =
      if v.index >= c then v.copy(index = v.index + d)
      else v
    map(onVar, c)

  // The substitution [j ↦ s] t of term s for numbered j in term t
  def termSubst(j: Int, s: BTerm): BTerm => BTerm =
    def onVar(v: BVar, c: Int): BTerm =
      if v.index == j + c then termShift(c)(s)
      else v
    map(onVar, 0)

  def termSubstTop(s: BTerm): BTerm => BTerm =
    (termSubst(0, termShift(1)(s))) andThen termShift(-1)

  def map(onVar: (BVar, Int) => BTerm, c: Int)(term: BTerm): BTerm =
    def walk(onVar: (BVar, Int) => BTerm, c: Int, t: BTerm): BTerm =
      t match
        case t @ BVar(_)  => onVar(t, c)
        case BAbs(t1)     => BAbs(walk(onVar, c + 1, t1))
        case BApp(t1, t2) => BApp(walk(onVar, c, t1), walk(onVar, c, t2))
    walk(onVar, c, term)

  def eval(env: Env, term: BTerm): BTerm =
    term match
      case BApp(t1, t2) =>
        val r = eval(env, t1)
        r match
          case BAbs(t12) =>
            eval(env, termSubstTop(t2)(t12))
          case _ =>
            BApp(r, t2)
      case _ =>
        term

  def norm(env: Env, term: BTerm, count: Int): Option[BTerm] =
    if count > 100000 then None
    else
      eval(env, term) match
        case BAbs(t) => norm(env, t, count + 1).map(BAbs(_))
        case BApp(t1, t2) =>
          for
            t11 <- norm(env, t1, count + 1)
            t22 <- norm(env, t2, count + 1)
          yield BApp(t11, t22)
        case t @ _ => Some(t)

  def norm(term: BTerm): BTerm =
    norm(Map.empty, term)

  def norm(env: Env, term: BTerm): BTerm =
    val t = eval(env, term)
    norm(env, t, 0) match
      case Some(t1) => t1
      case None     => t

class Interpreter:
  import Parser.{ Stmt, Term }
  import scala.collection.mutable.Map

  val env = Map[String, Term]()

  def load(program: String): Either[String, Unit] =
    for
      ts <- Lexer.scan(program)
      p  <- Parser.parse(Parser.program)(ts)
      _ = p.map(eval)
    yield ()

  def eval(input: String): String =
    val r = for
      ts <- Lexer.scan(input)
      t  <- Parser.parse(Parser.line)(ts)
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
        Evaluation.norm(env.toMap, bTerm).toString
      case Stmt(_, name, term) =>
        env += (name -> term)
        s"$name = $term"
