//> using scala "3.1.2"

package ulc

def loop =
  import scala.io.StdIn.readLine
  import Parser.{Term, Stmt}

  val interpreter = Interpreter()
  val quitCommands = List("exit", "quit", ":q")
  var input = ""

  println("Welcome to ulc repl!")
  println("Enter exit, quit or :q to quit")
  while
    input = readLine("λ> ")
    quitCommands.indexOf(input) == -1
  do
    val line = input match
      case s":load $s" =>
        val r = for
          str <- FileReader.read(s)
          _ <- interpreter.load(str)
        yield ()
        println(r.fold(identity, {_ => "Success!"}))
      case s =>
        println(interpreter.eval(s))

@main def main() = loop
