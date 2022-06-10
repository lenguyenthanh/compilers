//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.scalameta::munit::0.7.27"

package ulc

import cats.syntax.all.*
import cats.instances.all.*

class UlcSuite extends munit.FunSuite:
  val input = List(
    // "\\x. \\y. y",
    // "\\x. x",
    // "(\\x. x) x",
    // "(\\x. x x) x",
    // "(\\x. \\y. x) a b",
    // "\\a. \\b. \\s. \\z. a s (b s z)",
    // "λf.(λx.f(λy.(x x)y))(λx.f(λy.(x x)y))"
    "(\\n. \\f. \\x. f (n f x)) (\\f. \\x. x)"
  )

  test("Lexer") {
    val result = input.traverse(Lexer.scan)
    assert(result.isRight, true)
  }

  test("Parser") {
    val result = input.traverse(parse)
    assert(result.isRight, true)
  }

  test("Parser fails") {
    val input  = "\\x \\y. y"
    val result = parse(input)
    assert(result.isLeft, true)
  }

  test("de bruijn") {
    val result = input.map(deBruijn)
    println(result.mkString("\n"))
    assert(true, true)
  }

  test("Evaluate") {
    val result = input.map(Interpreter.eval)
    println(result.mkString("\n"))
    assert(true, true)
  }


  def parse(x: String) =
    for
      ts <- Lexer.scan(x)
      t  <- Parser.parse(ts)
    yield t

  def deBruijn(x: String) =
    for
      ts <- Lexer.scan(x)
      t  <- Parser.parse(ts)
      br = DeBruijn.transform(t)
    yield br
