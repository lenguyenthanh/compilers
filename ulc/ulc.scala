//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.typelevel::cats-parse:0.3.7"

package ulc

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

trait Parser[A, +B]:
  def parse(input: List[A]): Either[String, (List[A], B)]

  def |[B1 >: B](that: Parser[A, B1]): Parser[A, B1] = new Parser[A, B1] {
    def parse(input: List[A]) =
      Parser.this.parse(input) match
        case Left(_)         => that.parse(input)
        case r @ Right(_, _) => r
  }

object Parser:

  import cats.Functor
  import cats.Monad

  def unit[A, B](b: B): Parser[A, B] = new Parser[A, B] {
    def parse(input: List[A]) = Right(input, b)
  }

  given [A]: Functor[[x] =>> Parser[A, x]] with
    def map[B, C](p: Parser[A, B])(f: B => C): Parser[A, C] =
      new Parser[A, C] {
        def parse(input: List[A]) =
          p.parse(input).map { case (rest, a) => (rest, f(a)) }
      }

  given [A]: Monad[[x] =>> Parser[A, x]] = new Monad[[x] =>> Parser[A, x]] {

    def pure[B](b: B): Parser[A, B] = new Parser[A, B] {
      def parse(input: List[A]) = Right(input, b)
    }

    def flatMap[B, C](p: Parser[A, B])(f: B => Parser[A, C]): Parser[A, C] = new Parser[A, C] {
      def parse(input: List[A]) =
        p.parse(input).flatMap((rest, b) => f(b).parse(rest))
    }

    // @tailrec is this a hack
    def tailRecM[B, C](b: B)(f: B => Parser[A, Either[B, C]]): Parser[A, C] = new Parser[A, C] {
      def parse(input: List[A]) =
        f(b).parse(input) match
          case Left(err)               => Left(err)
          case Right((rest, Right(c))) => Right(rest, c)
          case Right((rest, Left(b1))) => tailRecM(b1)(f).parse(rest)
    }
  }

  import cats.instances.all.*
  import cats.syntax.all.*
  import cats.data.NonEmptyList
  extension [A, B](p: Parser[A, B])
    def many: Parser[A, List[B]] = many1.map(_.toList) | Parser.unit[A, List[B]](Nil)
    def many1: Parser[A, NonEmptyList[B]] =
      for
        first <- p
        rest  <- p.many
      yield NonEmptyList(first, rest)

object Parser1:
  import scala.reflect.Typeable

  import Token.*
  import Parser.*
  import Parser.given
  import cats.instances.all.*
  import cats.syntax.all.*

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

  def token[T: Typeable]: Parser[Token, T] = new Parser[Token, T] {
    def parse(input: List[Token]): Either[String, (List[Token], T)] =
      // println(s"token ${input.length}")
      input match
        case head :: tail =>
          head match
            case t: T => Right(tail, t)
            case _    => Left("unexpected token")
        case _ => Left("Empty input")
  }

  import Term.*
  import cats.data.NonEmptyList
  def term: Parser[Token, Term]           = lambda | group | app | variable
  def termWithoutApp: Parser[Token, Term] = lambda | group | variable
  lazy val variable: Parser[Token, Var] =
    token[Identifier].map(id => Var(id.info, id.lexeme))

  lazy val lambda: Parser[Token, Term] =
    for
      l <- token[Lambda]
      // _ = println(l)
      id <- variable
      // _ = println(id)
      _ <- token[Dot]
      t <- app
      // _ = println(t)
    yield Abs(l.info.merge(t.info), id, t)

  lazy val app: Parser[Token, App] = termWithoutApp.many1.map(ts => App(mergeInfo(ts), ts.toList))

  def mergeInfo(ts: NonEmptyList[Term]): Info =
    ts.foldLeft(ts.head.info)((a, b) => a.merge(b.info))

  // println("app")
  // for
  //   t1 <- termWithoutApp
  //   t2 <- termWithoutApp
  // yield TMApp(t1.info.merge(t2.info), t1, t2)

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

@main
def main =
  import Term.*
  import Evaluation.*
  val x = "\\x. \\y. y"
  // val x  = "\\x. x"
  // val x = "x \\x."
  // val x = "\\a. \\b. \\s. \\z. a s (b s z)"
  // val x = "λf.(λx.f(λy.(x x)y))(λx.f(λy.(x x)y))"
  val t = for
    ts <- Lexer.scan(x)
    // _ = println(s"ts: ${ts.mkString("\n")}")
    _ = println(s"ts: ${ts.length}")
    t <- Parser1.parse(ts)
  yield t
  println()
  println(t)
  // (λ.1 0 2) (λ.0) -> 0 (λ.0) 1
  // val t = TMApp(
  //   (),
  //   TMAbs((), TMApp((), TMApp((), TMVar((), 1, 3), TMVar((), 0, 3)), TMVar((), 2, 3))),
  //   TMAbs((), TMVar((), 0, 1))
  // )
  // println(t)
  // println(eval(t))
