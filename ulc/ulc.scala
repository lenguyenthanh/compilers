//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.typelevel::cats-parse:0.3.7"

package ulc

import cats.*
import cats.syntax.all.*
import cats.instances.all.*
import cats.data.NonEmptyList

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
    case Lambda(override val info: Info)     extends Token("\\", info)
    case Dot(override val info: Info)                                     extends Token(".", info)
    case Identifier(override val lexeme: String, override val info: Info) extends Token(lexeme, info)

  val endOfLine: P[Unit]    = R.cr | R.lf
  val whitespaces: P0[Unit] = P.until0(!R.wsp).void
  val location              = P.caret.map(c => Location(c.line, c.col, c.offset))

  // token
  val leftParen  = P.char('(').info.map(p => LeftParen(p._2))
  val rightParen = P.char(')').info.map(p => RightParen(p._2))
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

  import parser.Parser as P
  import parser.Parser.given
  import Lexer.Token.*
  import Lexer.Token
  import Term.*

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

  def term: P[Token, Term]           = app
  def termWithoutApp: P[Token, Term] = lambda | group | variable

  lazy val variable: P[Token, Var] =
    token[Identifier].map(id => Var(id.info, id.lexeme))

  lazy val lambda: P[Token, Term] =
    for
      l  <- token[Lambda]
      id <- variable
      _  <- token[Dot]
      t  <- app
    yield Abs(l.info.merge(t.info), id, t)

  lazy val app: P[Token, Term] = termWithoutApp.many1.map(collapse)

  def collapse(ts: NonEmptyList[Term]): Term =
    ts match
      case NonEmptyList(head, Nil) => head
      case NonEmptyList(head, x::Nil) => App(head.info.merge(x.info), head, x)
      case NonEmptyList(head, x::y::xs) => App(mergeInfo(ts), App(head.info.merge(x.info), head, x), collapse(NonEmptyList(y, xs)))

  def mergeInfo(ts: NonEmptyList[Term]): Info =
    ts.foldLeft(ts.head.info)((a, b) => a.merge(b.info))

  lazy val group =
    for
      _ <- token[LeftParen]
      t <- term
      _ <- token[RightParen]
    yield t

  def parse(tokens: List[Token]): Either[String, Term] =
    term.parse(tokens) match
      case Left(err)          => Left(err)
      case Right((Nil, term)) => Right(term)
      case _                  => Left("Partial Parse")

object DeBruijn:
  import BTerm.*
  import Parser.Term.*
  import Parser.Term
  import scala.collection.mutable.ListBuffer

  def transform: Term => BTerm =
    // TODO mutation doesn't work
    val free = ListBuffer[String]()
    def go(ctx: List[String]): Term => BTerm =
      case Var(fi, x) =>
        val idx =  ctx.indexOf(x)
        if idx == -1 then
          val freeIdx = free.indexOf(x)
          if freeIdx == -1 then
            free += x
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

  def eval1(term: Term): (Term, Boolean) =
    term match
      case BApp(fi, BAbs(_, t12), v2) =>
        (termSubstTop(v2)(t12), false)
      // TODO fix - so ugly
      case BApp(fi, t1, t2) =>
        val r1 = eval1(t1)
        if r1._2 then
          val r2 = eval1(t2)
          (BApp(fi, r1._1, r2._1), r2._2)
        else
          (BApp(fi, r1._1, t2), r1._2)
      case BAbs(fi, t) =>
        val r = eval1(t)
        (BAbs(fi, r._1), r._2)
      case _ => (term, true)

  def eval(term: Term): Term =
    val t1 = eval1(term)
    t1 match
      case (t, false) => eval(t)
      case (t, true)  => t

object Interpreter:
  def eval(input: String): String =
    val r = for
      ts <- Lexer.scan(input)
      _ = println(ts)
      t <- Parser.parse(ts)
      _ = println(t)
      br = DeBruijn.transform(t)
      _ = println(br)
      result = Evaluation.eval(br)
    yield result.toString
    r.fold(identity, identity)

def loop() =
    import scala.io.StdIn.readLine
    val quitCommands = List("exit", "quit", ":q")
    var input = ""
    println("Welcome to ulc repl!")
    println("Enter exit, quite or :q to quit")
    while
      input = readLine("λ> ")
      quitCommands.indexOf(input) == -1
    do println(Interpreter.eval(input))

@main def main() = loop()
