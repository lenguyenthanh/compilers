//> using scala "3.2.0"
//> using lib "org.typelevel::cats-core::2.8.0"
//> using lib "org.typelevel::cats-parse::0.3.8"

package tlc

case class Location(val line: Int, val col: Int, val offset: Int)
case class Info(start: Location, end: Location):
  def merge(other: Info): Info =
    copy(end = other.end)

case class Parsed[A](info: Info, value: A)

enum Type:
  case Int
  case Bool
  case Arrow(input: Type, output: Type)

enum Term:
  case True
  case False
  case If(cond: Term, ifTrue: Term, ifFalse: Term)
  case Var(index: Int)
  case Abs(ty: Type, body: Term)
  case App(t1: Term, t2: Term)

enum Binding:
  case Name
  case Type(ty: Type)

type Context = List[Binding]

object Parser:

  enum Term:
    case True
    case False
    case If(cond: Term, ifTrue: Term, ifFalse: Term)
    case Var(name: String)
    case Abs(arg: Var, val ty: Type, term: Term)
    case App(t1: Term, t2: Term)

