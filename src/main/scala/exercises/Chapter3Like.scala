package exercises

import scala.annotation.tailrec

/**
  * Created by tomas.mccandless on 2019-02-21.
  */
trait Chapter3Like {

  @tailrec
  final def drop[A](l: List[A], n: Int): List[A] = {

    if (n <= 0) l
    else if (n > l.length) Nil
    else drop(l.tail, n - 1)
  }


  @tailrec
  final def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {

    l match {
      case Nil => Nil
      case x :: xs if f(x) => dropWhile(xs)(f)
      case other => other
    }
  }


  def init[A](l: List[A]): List[A] = {

    @tailrec
    def init(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil => acc
        case x1 :: _ :: Nil => x1 :: acc
        case x1 :: xs => init(xs, x1 :: acc)
      }
    }

    init(l, Nil).reverse
  }


  def foldRight[A,B](as: List[A], zero: B)(f: (A, B) => B): B = as match {
    case Nil => zero
//    case x :: xs => f(x, foldRight(xs, zero)(f))
    case x :: xs => foldRight(xs, f(x,zero))(f)
  }

  val x = foldRight(List(1,2,3), Nil:List[Int])(_ :: _)
  println(x)


  def length[A](as: List[A]): Int = this.foldRight(as, 0)((_, acc) => acc + 1)


  @tailrec
  final def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

}
