package exercises

import exercises.Par.Par
import exercises.Translate.~>

import scala.annotation.tailrec
import scala.language.higherKinds


/**
  *
  * Created by tdm on 2019-07-28.
  */
sealed trait Free[F[_], A] { self =>

//  def run(): A

  // 13.1
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))


  def flatMap[B](f: A => Free[F,B]): Free[F,B] =
    FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

object Free {


  type Id[X] = X


  // 13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }

  def apply[F[_], A](a: => A): Free[F, A] = freeMonad[F].unit(a)

  def ReadLine: Free[Id, String] = Free {
    readLine()
  }

  def PrintLine(msg: String): Free[Id, Unit] = Free {
    println(msg)
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: Free[Id, Unit] = for {
    _ <- PrintLine("enter fahrenheit degrees:")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()


  val echo: Free[Id, Unit] = ReadLine.flatMap(PrintLine)

  val readInt: Free[Id, Int] = ReadLine.map(_.toInt)

}

object Interpreter {

  type TailRec[A] = Free[Function0,A]
  type Async[A] = Free[Par,A]

//  @tailrec def step[A](async: Free[A]): Free[A] = async match {
//    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//
//  @tailrec def run[A](io: Free[A]): Par[A] = io match {
//    case Return(a) => Par.unit(a)
//    case Suspend(r) => Par.flatMap(r)(a => run(a))
//    case FlatMap(x, f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
//      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//    }
//  }

  val interpreter = Free.freeMonad[Function0]

  // 13.2
  @tailrec def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x: Free[Function0, A], f: (A => Free[Function0, A])) => x match {
      case Return(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMap(y: Free[Function0, A], g: (A => Free[Function0, A])) => runTrampoline { y flatMap { a => g(a) flatMap f } }
    }
  }

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(a))
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  // 13.3
  def stepGeneral[F[_], A](a: Free[F, A])(implicit mf: Monad[F]): Free[F, A] = {
    case FlatMap(FlatMap(x,f), g) => stepGeneral(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => stepGeneral(f(x))
    case _ => a
  }

  def runGeneral[F[_],A](a: Free[F,A])(implicit mf: Monad[F]): F[A] = a match {
    case Return(v) => mf.unit(v)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => mf.flatMap(r)(a => runGeneral(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
    stepGeneral(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }


  // 13.4
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] =
    fg.apply(runFree(f)(fg)(Free.freeMonad[G]))


}

