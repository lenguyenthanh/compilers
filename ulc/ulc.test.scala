//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"
//> using lib "org.scalameta::munit::0.7.27"

package ulc

import cats.syntax.all.*
import cats.instances.all.*

class UlcSuite extends munit.FunSuite:
  val input = List(
    "\\x. \\y. y",
    "\\x. x",
    "x \\x. x",
    "\\a. \\b. \\s. \\z. a s (b s z)",
    "λf.(λx.f(λy.(x x)y))(λx.f(λy.(x x)y))"
  )

  test("Lexer") {
    val result = input.traverse(Lexer.scan)
    assert(result.isRight, true)
  }

  test("Parser") {
    val result = input.traverse(parse)
    println(result)
    assert(result.isRight, true)
  }

  test("Parser fails") {
    val input = "\\x \\y. y"
    val result = parse(input)
    assert(result.isLeft, true)
  }

  def parse(x: String) =
    for
      ts <- Lexer.scan(x)
      t <- Parser.parse(ts)
    yield t
