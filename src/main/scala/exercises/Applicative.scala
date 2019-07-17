package exercises

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  *
  * Created by tdm on 2019-07-16.
  */
trait Applicative[F[_]] extends Functor[F] {

  // primitives
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]


  // derived
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    // ignores second arg
    map2(fa, this.unit(()))((a, _) => f(a))
  }


  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(this.unit(List.empty[B])) { case (a, fbs) =>
        this.map2(f(a), fbs)(_ :: _)
    }
  }

  // 12.1
  @tailrec
  final def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    val reversed = lma.foldLeft(this.unit(List.empty[A])) { case (acc: F[List[A]], a: F[A]) =>
      this.map2(a, acc)(_ :: _)
    }

    this.map(reversed)(_.reverse)
  }

  // 11.4
  // almost the opposite of fold
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    this.map(ma)(a => List.fill(n)(a))
  }




  // 12.2
  // alternate primitive with unit
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def map2viaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fbc: F[B => C] = this.apply(this.unit(f.curried))(fa)
    this.apply(fbc)(fb)
  }

  // 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbcd: F[B => C => D] = this.apply(this.unit(f.curried))(fa)
    val fcd: F[C => D] = this.apply(fbcd)(fb)
    this.apply(fcd)(fc)
  }

  def map4[A, B, C, D, E](
                         fa: F[A],
                         fb: F[B],
                         fc: F[C],
                         fd: F[D]
                         )(f: (A, B, C, D) => E): F[E] = {
    val fbcde: F[B => C => D => E] = this.apply(this.unit(f.curried))(fa)
    val fcde: F[C => D => E] = this.apply(fbcde)(fb)
    val fde: F[D => E] = this.apply(fcde)(fc)
    this.apply(fde)(fd)
  }
}


object Applicatives {

  def validationApplicative[E]: Applicative[({
    type f[x] = Validation[E, x]
  })#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Invalid(h1, t1), Invalid(h2, t2)) => Invalid(h1, (t1 :+ h2) ++ t2)
        case (i@ Invalid(_, _), Valid(_)) => i
        case (Valid(_), i@ Invalid(_, _)) => i
//        case (Valid(a1), Valid(a2)) => Try(f(a1, a2)) match {
//          case Success(c) => Valid(c)
//          case Failure(err) => Invalid(err)
//        }
        case (Valid(a1), Valid(a2)) => Valid(f(a1, a2))
      }

    }

    override def unit[A](a: => A): Validation[E, A] = Validation(a)
  }
}