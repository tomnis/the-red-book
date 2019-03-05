package exercises

/**
  *
  * Created by tdm on 2019-03-04.
  */

sealed trait FpEither[+E, +A] {
  def map[B](f: A => B): FpEither[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(value) => Right(f(value))
    }
  }


  def flatMap[EE >: E, B](f: A => FpEither[EE, B]): FpEither[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(value) => f(value)
    }
//    this.map(f).orElse()
  }

  def orElse[EE >: E,B >: A](b: => FpEither[EE, B]): FpEither[EE, B] = {
    this match {
      case Left(e) => b
      case Right(value) => this
    }
  }


  def map2[EE >: E, B, C](b: FpEither[EE, B])(f: (A, B) => C): FpEither[EE, C] = {
    for {
      r <- this
      r2 <- b
    } yield f(r, r2)
  }
}

case class Left[+E](value: E) extends FpEither[E, Nothing]
case class Right[+A](value: A) extends FpEither[Nothing, A]
