package exercises

import scala.annotation.tailrec

/**
  * Created by tomas.mccandless on 2019-02-21.
  */
trait Chapter3Like extends Chapter2 {

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


  // 3.6
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

  // 3.8
//  println(foldRight(List(1,2,3), Nil:List[Int])(_ :: _))


  // 3.9
  def length[A](as: List[A]): Int = this.foldRight(as, 0)((_, acc) => acc + 1)


  // 3.10
  @tailrec
  final def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }



  def sumFold(nums: List[Int]): Int = this.foldLeft(nums, 0)(_  + _)


  def productFold(nums: List[Int]): Int = this.foldLeft(nums, 1)(_ * _)

  def lengthFold(nums: List[Int]): Int = this.foldLeft(nums, 0)((acc, _) => acc + 1)



  // 3.12
  def reverse[A](as: List[A]): List[A] = {
    this.foldRight(as, List.empty[A])(_ :: _ )
  }




  // 3.13
  def foldRightInTermsOfFoldLeft[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    this.foldLeft(as.reverse, zero){ case (b, a) => f(a, b) }
  }


  // 3.14
  def append[A](as: List[A], moreAs: List[A]): List[A] = {
    as match {
      case Nil => moreAs
      case x :: xs  => x :: append(xs, moreAs)
    }
  }


  // 3.15
  def flatten[A](as: List[List[A]]): List[A] = {

    as match {
      case Nil => Nil
      case Nil :: xs => this.flatten(xs)
      case xs :: ys => this.append(xs, this.flatten(ys))
    }
  }


  // 3.16
  def increment(nums: List[Int]): List[Int] = {
    val x: List[Int] = this.foldLeft(nums, List.empty[Int]) { case (acc, a) => (a + 1) :: acc }
    x.reverse
  }


  def asString(nums: List[Double]): String = {
    this.foldLeft(nums, "") { case (acc, a) => acc + ", " + a.toString}
  }


  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    this.foldLeft(as, List.empty[B]){ case (acc, a) => f(a) :: acc }.reverse
  }


  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case x :: xs if f(x) => x :: this.filter(xs)(f)
      case _ :: xs => this.filter(xs)(f)
    }
  }


  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    this.flatten(this.map(as)(f))
  }


  // 3.22
  // assumes same length
  def add(as: List[Int], bs: List[Int]): List[Int] = {

    @tailrec def go(cs: List[Int], ds: List[Int], acc: List[Int]): List[Int] = {
      if (cs.isEmpty || ds.isEmpty) acc
      else go(cs.tail, ds.tail, (cs.head + ds.head) :: acc)
    }

    go(as, bs, List.empty).reverse
  }


  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @tailrec def go(cs: List[A], ds: List[B], acc: List[C]): List[C] = {
      if (cs.isEmpty || ds.isEmpty) acc
      else go(cs.tail, ds.tail, f(cs.head, ds.head) :: acc)
    }

    go(as, bs, List.empty).reverse
  }


  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup.sliding(sub.length).contains(sub)
  }

  @tailrec
  final def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    if (sub.isEmpty) true
    else if (sup.isEmpty && sub.nonEmpty) false
    else if (sup.head != sub.head) false
    else startsWith(sup.tail, sub.tail)
  }


  // 3.25
  // TODO how to make it tailrec
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(left, right) => this.maximum(left) max this.maximum(right)
    }
  }


  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => (this.depth(left) max this.depth(right)) + 1
    }
  }


  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(this.map(left)(f), this.map(right)(f))
    }
  }
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]