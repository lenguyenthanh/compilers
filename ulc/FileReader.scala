//> using scala "3.3.0"

package ulc

import scala.io.Source
import scala.util.{ Failure, Success, Try }

object FileReader:
  def read(path: String): Either[String, String] =
    Try {
      val bufferedSource = Source.fromFile(path)
      val content        = bufferedSource.getLines.filter(!_.isEmpty).mkString("\n")
      bufferedSource.close
      content
    } match {
      case Success(v) => Right(v)
      case Failure(_) => Left(s"Cannot load file $path")
    }
