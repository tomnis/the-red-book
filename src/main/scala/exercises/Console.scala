package exercises

import exercises.Par.Par

import scala.language.higherKinds

/**
  *
  * Created by tdm on 2019-07-30.
  */
sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar: Par[Option[String]] = Par.lazyUnit(run)

  def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try Option(readLine())
    catch {
      case e: Exception => None
    }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar: Par[Unit] = Par.lazyUnit(println(line))
  def toThunk: () => Unit = () => println(line)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Translate {
  type ~>[F[_], G[_]] = Translate[F,G]

  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }
}