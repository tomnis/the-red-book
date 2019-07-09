package exercises

import language.higherKinds
import language.implicitConversions
import scala.util.matching.Regex

// note self
trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // primitive
  implicit def regex(r: Regex): Parser[String]
  // primitive
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString).map((s: String) => s.charAt(0))

  // primitive
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  // primitive
  def slice[A](p: Parser[A]): Parser[String]

  // zero or more
  def many[A](p: Parser[A]): Parser[List[A]] = many1(p).or(succeed(Nil))

  // one or more
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p)) { case (a, as) => a :: as }

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2) map { case (a, b) => f(a, b) }

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  // primitive
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  // place primary definitions directly in Parsers and delegate in ParserOps to that primary definition
  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    // 9.1
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def succeed(a: A): Parser[A] = string("").map(_ => a)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }


  object Laws {
    import Prop.forAll
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}

case class ParseError(stack: List[(Location, String)])