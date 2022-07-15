//> using scala "3.1.3"
//> using lib "org.typelevel::cats-core::2.8.0"
//> using lib "org.typelevel::cats-parse::0.3.8"

package tlc

enum Ty:
  case Bool
  case Arr(input: Ty, output: Ty)
