package exercises

import scala.language.higherKinds

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


  // 12.2
  // alternate primitive with unit
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

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
