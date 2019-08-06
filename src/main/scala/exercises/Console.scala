package exercises

import exercises.Par.Par

import scala.language.higherKinds

/**
  * Algebraic data type that only models interaction with the console.
  *
  * Created by tdm on 2019-07-30.
  */
sealed trait Console[A] {
  def toPar: Par[A] = Par.lazyUnit(run)
  def toThunk: () => A = () => run

  def run: A
}

case object ReadLine extends Console[Option[String]] {
  override def run: Option[String] = {
    try Option(readLine())
    catch {
      case e: Exception => None
    }
  }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def run: Unit = println(this.line)
}




// Console[A] represents a computation that yields an A
// only 2 forms, readline and println
// embed Console into Free
object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
}


// how do we actually run a Console?
// we need a Monad[Console]
// why isnt it possible to implement flatMap for Console?



trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Translate {
  val consoleToFunction0 = new Translate[Console, Function0] {
    override def apply[A](a: Console[A]): Function0[A] = a.toThunk
  }
  val consoleToPar = new Translate[Console, Par] {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }
}