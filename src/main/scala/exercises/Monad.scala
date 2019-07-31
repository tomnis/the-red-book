package exercises

import java.util.concurrent.Executors

import exercises.Par.Par

import scala.language.higherKinds

/**
  * Results of previous computations may influence which computations
  * are next. Contrasted with Applicative, where the structure of
  * computation is fixed.
  *
  * Created by tdm on 2019-07-15.
  */
trait Monad[F[_]] extends Applicative[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    this.flatMap(fa)(a => this.map(fb)(b => f(a, b)))
  }

  // 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A])) { case (x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x)
    }
  }


  // 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a: A => {
      this.flatMap(f(a))(g)
    }
  }


  // 11.8
  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    this.compose((_:Unit) => fa, f)(())
  }


  // 11.11
  // compose(f, unit) == f
  // a => flatMap(f(a))(unit)
  // a =>
  // compose(unit, f) == f


  // 11.12
  def join[A](mma: F[F[A]]): F[A] = {
    this.flatMap(mma)((ma: F[A]) => ma)
  }

  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = flatMap(a)(_ => t)
    t
  }
}


object Monads {


  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }


  // 11.1
  private val es = Executors.newFixedThreadPool(4)
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = {
      Par.map(fa)(a => Par.run(es)(f(a)))
    }
  }


//  val parserMonad = new Monad[Parsers] {
//    override def unit[A](a: => A): Parsers[A] = ???
//
//    override def flatMap[A, B](fa: Parsers[A])(f: A => Parsers[B]): Parsers[B] = ???
//  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa.flatMap(f)
    }
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = {
      fa.flatMap(f)
    }
  }


  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
      fa.flatMap(f)
    }
  }


  // 11.2
//  trait S[A, B, C <: Tuple2[B, C]] extends State[A, B] { }

//  val stateMonad = new Monad[State] {
//    override def unit[A: Tuple2](a: => A): State[A] = ???
//
//    override def flatMap[A, B](fa: State[A])(f: A => State[B]): State[B] = ???
//  }

  // 11.17
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }


  object IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
    override def unit[A](a: => A): State[Int, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[Int, A])(f: A => State[Int, B]): State[Int, B] = fa.flatMap(f)
  }


  // 11.20
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader { r: R =>
        val b: A = fa.run(r)
        f(b).run(r)
      }
    }
  }


  // 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      fa.flatMap(f)
    }
  }
}


case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}


case class Reader[R, A](run: R => A)
