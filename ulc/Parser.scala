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

  def |[C](that: Parser[A, C]): Parser[A, B | C] = new Parser[A, B | C] {
    def parse(input: List[A]) =
      Parser.this.parse(input) match
        case Left(_)         => that.parse(input)
        case r @ Right(_, _) => r
  }

object Parser:

  def unit[A, B](b: B): Parser[A, B] = new Parser[A, B] {
    def parse(input: List[A]) = Right(input, b)
  }

  def head[A]: Parser[A, A] = new Parser[A, A] {
    def parse(input: List[A]) =
      input match
        case head :: tail => Right(tail, head)
        case _            => Left("Empty input")
  }

  def withFilter[A](filter: A => Boolean): Parser[A, A] = new Parser[A, A] {
    def parse(input: List[A]): Either[String, (List[A], A)] =
      input match
        case head :: tail =>
          if filter(head) then Right(tail, head)
          else Left(s"unexpected token $head")
        case _ => Left("Empty input")
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

    // @tailrec todo do we need tailrec? is this a hack?
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

    def ~[C](that: Parser[A, C]): Parser[A, (B, C)] =
      for
        b <- p
        c <- that
      yield (b, c)

    def *>[C](that: Parser[A, C]): Parser[A, C] =
      for
        _ <- p
        c <- that
      yield c

    def <*[C](that: Parser[A, C]): Parser[A, B] =
      for
        b <- p
        _ <- that
      yield b

    def ? : Parser[A, Option[B]] =
      input =>
        p.parse(input) match
          case Right(rest, b) => Right(rest, Some(b))
          case Left(_)        => Right(input, None)
