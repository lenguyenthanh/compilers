//> using scala "3.3.3"

package ulc

def loop =
  import scala.io.StdIn.readLine
  import Parser.{ Term, Stmt }

  val interpreter  = Interpreter()
  val quitCommands = List(":quit", ":q")
  var input        = ""

  val welcome = """
  |Welcome to ulc repl!
  |Enter :load or :l and then file name to load a program
  |Enter :quit or :q to quite the repl
  """.stripMargin
  println(welcome)
  while {
    input = readLine("λ> ")
    quitCommands.indexOf(input) == -1
  }
  do
    val line = input match
      case s":load $s" =>
        load(s)
      case s":l $s" =>
        load(s)
      case s =>
        println(removeRedundantParent(interpreter.eval(s)))

  def load(path: String) =
    val r = for
      str <- FileReader.read(path)
      _   <- interpreter.load(str)
    yield ()
    println(r.fold({ s => s"Load failed $path: $s" }, { _ => s"Loaded $path!" }))

def removeRedundantParent(str: String) =
  str match
    case s"($s)" => s
    case _       => str

@main def main() = loop
