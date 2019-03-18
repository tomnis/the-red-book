package exercises

import scala.annotation.tailrec
import Stream._

/**
  *
  * Created by tdm on 2019-03-11.
  */
sealed trait Stream[+A] {

  def toList: List[A] = {
    @tailrec def go(as: Stream[A], acc: List[A]): List[A] = {
      as match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }

    go(this, List.empty[A]).reverse
  }


  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }


  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons[A](h(), t().take(n - 1))
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        val realHead: A =  h()
        if (p(realHead)) cons[A](realHead, t().takeWhile(p))
        else Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)


  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)


  // 5.5
  def takeWhileInTermsOfFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  // 5.6 headoption using foldright
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Option(a))
  }


  // 5.7 map, filter, append, and flatMap using foldRight
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }


  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if(f(a)) cons(a, b) else b)
  }


  // stuck
  def append[B >: A](as: => Stream[B]): Stream[B] = {
    foldRight(as)((a, b) => cons(a, b))
  }


  // flatMap
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }


  // 5.13
//  def mapUnfold[B](f: A => B): Stream[B] = {
//    Stream.unfold(this) {
//      case Cons(a, b) => Option((f(a), b()))
//      case _ => None
//    }
//  }


  def takeUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(a, b), num) if num > 0 => Option(a(), (b().takeUnfold(n - 1), n - 1))
      case _ => None
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(a, b) if p(a()) => Option((a(), b()))
      case _ => None
    }
  }


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold(this, s2) {
      case (Cons(a1, b1), Cons(a2, b2)) => Option((Option(a1()), Option(a2())), (b1(), b2()))
      case (Empty, Cons(a2, b2)) => Option((None, Option(a2())), (empty[A], b2()))
      case (Cons(a1, b1), Empty) => Option((Option(a1()), None), (b1(), empty[B]))
      case _ => None
    }
  }


}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  // infinite stream
  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }


  def fibs: Stream[Int] = {
    def go(fibn2: Int, fibn1: Int): Stream[Int] = {
      cons(fibn2, go(fibn1, fibn2 + fibn1))
    }

    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }
  }

  // 5.12 fibs, from, constant, and ones in terms of unfold.
  def onesUnfold: Stream[Int] = {
    unfold(1)(s => Option(1, s))
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Option(a, s))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Option(s, s + 1))
  }

  def fibsUnfold: Stream[Int] = {
    unfold((0, 1)) { case (f0, f1) => Option(f0, (f1, f0 + f1)) }
  }
}
