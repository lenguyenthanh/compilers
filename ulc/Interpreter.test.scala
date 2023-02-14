//> using scala "3.2.0"
//> using lib "org.typelevel::cats-core:2.9.0"
//> using lib "org.scalameta::munit::0.7.27"

package ulc

import cats.syntax.all.*
import cats.instances.all.*

class UlcSuite extends munit.FunSuite:
  val input = List(
    "\\x. \\y. y",
    "\\x. x",
    "(\\x. x) x",
    "(\\x. x x) x",
    "(\\x. \\y. x) a b",
    "\\a. \\b. \\s. \\z. a s (b s z)",
    "(\\n. \\f. \\x. f (n f x)) (\\f. \\x. x)",
    "λf.(λx.f(λy.(x x)y))(λx.f(λy.(x x)y))"
  )

  val stmt = List(
    "y = \\x. x"
  )

  test("Lexer") {
    val result = input.traverse(Lexer.scan)
    assert(result.isRight, true)
  }

  test("Parser") {
    val result = input.traverse(parse)
    assert(result.isRight, true)
  }

  test("Parser with line") {
    val result = stmt.traverse(lineParse)
    assert(result.isRight, true)
  }

  test("Parser fails") {
    val input  = "\\x \\y. y"
    val result = parse(input)
    assert(result.isLeft, true)
  }

  test("de bruijn") {
    val result = input.map(deBruijn)
    assert(true, true)
  }

  test("Factorial") {
    val interpreter = Interpreter()
    val input = List(
      "T = λx. λy. x",
      "F = λx. λy. y",
      "0 = λf. λx. x",
      "1 = λf. λx. f x",
      "2 = λf. λx. f (f x)",
      "4 = λf. λx. f ( f (f (f x)))",
      "+ = λm. λn. λf. λx. m f (n f x)",
      "* = λm. λn. m (+ n) 0",
      "6 = + 4 2",
      "24 = * 4 6",
      "is0 = λn. n (λx. F) T",
      "pair = λf. λs. λb. b f s",
      "fst = λp. p T",
      "snd = λp. p F",
      "zz = pair 0 0",
      "ss = λp. pair (snd p) (+ 1 (snd p))",
      "prd = λm. fst (m ss zz)",
      "Y = λf.(λx.f(λy.(x x)y))(λx.f(λy.(x x)y))",
      "fact' = λr. λn. (is0 n) 1 (* n (r (prd n)))",
      "fact = λn. Y fact\' n"
    )
    input.foreach(interpreter.eval)
    val f2  = interpreter.eval("prd 2")
    val two = interpreter.eval("1")
    assertEquals(f2, two)
    val f4         = interpreter.eval("fact 4")
    val twentyFour = interpreter.eval("24")
    assertEquals(f2, two)
  }

  val lineParser = Parser.parse(Parser.line)
  val termParser = Parser.parse(Parser.app)
  def parse(x: String) =
    for
      ts <- Lexer.scan(x)
      t  <- termParser(ts)
    yield t

  def lineParse(x: String) =
    for
      ts <- Lexer.scan(x)
      t  <- lineParser(ts)
    yield t

  def deBruijn(x: String) =
    for
      ts <- Lexer.scan(x)
      t  <- termParser(ts)
      br = DeBruijn.transform(Map.empty, List())(t)
    yield br
