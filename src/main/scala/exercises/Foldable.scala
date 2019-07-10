package exercises

/**
  *
  * Created by tdm on 2019-07-09.
  */
trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(zero: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(zero: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.combine)

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])((a, b) => a :: b)
  }
}


// 10.12
object ListIsFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = as.foldRight(zero)(f)

  override def foldLeft[A, B](as: List[A])(zero: B)(f: (B, A) => B): B = as.foldLeft(zero)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.combine(b, f(a)))
}


object IndexedSeqIsFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(zero: B)(f: (A, B) => B): B = as.foldRight(zero)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(zero: B)(f: (B, A) => B): B = as.foldLeft(zero)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoids.foldMapV(as, mb)(f)
}


import scala.collection.immutable.{Stream => StdLibStream}
object StreamIsFoldable extends Foldable[StdLibStream] {
  override def foldRight[A, B](as: StdLibStream[A])(zero: B)(f: (A, B) => B): B = as.foldRight(zero)(f)

  override def foldLeft[A, B](as: StdLibStream[A])(zero: B)(f: (B, A) => B): B = as.foldLeft(zero)(f)

  override def foldMap[A, B](as: StdLibStream[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.combine(b, f(a)))
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeIsFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(zero: B)(f: (A, B) => B): B = as match {
    case Leaf(v) => f(v, zero)
    case Branch(left, right) => foldRight(left)(foldRight(right)(zero)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(zero: B)(f: (B, A) => B): B = as match {
    case Leaf(v) => f(zero, v)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(zero)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => mb.combine(mb.zero, f(v))
    case Branch(left, right) => mb.combine(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }
}


object OptionIsFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(zero: B)(f: (A, B) => B): B = as.foldRight(zero)(f)

  override def foldLeft[A, B](as: Option[A])(zero: B)(f: (B, A) => B): B = as.foldLeft(zero)(f)

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(v) => f(v)
  }
}
