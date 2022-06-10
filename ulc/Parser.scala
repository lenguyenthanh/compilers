//> using scala "3.1.2"
//> using lib "org.typelevel::cats-core:2.7.0"

package parser

import cats.Functor
import cats.Monad
import cats.syntax.all.*
import cats.instances.all.*
import cats.data.NonEmptyList

trait Parser[A, +B]:
  def parse(input: List[A]): Either[String, (List[A], B)]

  def |[B1 >: B](that: Parser[A, B1]): Parser[A, B1] = new Parser[A, B1] {
    def parse(input: List[A]) =
      Parser.this.parse(input) match
        case Left(_)         => that.parse(input)
        case r @ Right(_, _) => r
  }

object Parser:

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

  extension [A, B](p: Parser[A, B])
    def many: Parser[A, List[B]] = many1.map(_.toList) | Parser.unit[A, List[B]](Nil)
    def many1: Parser[A, NonEmptyList[B]] =
      for
        first <- p
        rest  <- p.many
      yield NonEmptyList(first, rest)

