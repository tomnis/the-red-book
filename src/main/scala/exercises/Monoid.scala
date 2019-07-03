package exercises

import exercises.Par.Par

/**
  *
  * Created by tdm on 2019-07-03.
  */
trait Monoid[A] {

  /** Satisfies associativity law. */
  def op(a1: A, a2: A): A

  /** Satisfies identity law. */
  def zero: A
}



object Monoids {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override val zero = ""
  }


  // why is this a def?
  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override val zero = Nil
  }


  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }


  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }


  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }


  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }


  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    // TODO does this satisfy associativity?
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }


   // 10.3
   def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
     private[this] def id: A => A = (a: A) => a
     // TODO check order
     override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

     override def zero: A => A = id
   }


  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)


  // 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)


  // 10.6
  def foldLeftViaFoldMap[A](as: List[A], zero: A)(op: (A, A) => A): A = {

    val m = new Monoid[A] {
      override def op(a1: A, a2: A): A = op(a1, a2)
      override def zero: A = zero
    }

    foldMap(as, m)((a: A) => a)
  }


  def foldRightViaFoldMap[A](as: List[A], zero: A)(op: (A, A) => A): A = {

    val m = new Monoid[A] {
      override def op(a1: A, a2: A): A = op(a1, a2)
      override def zero: A = zero
    }

    foldMap(as.reverse, m)((a: A) => a)
  }



  // 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v.head)
    else {
      // split in two
      val (left, right) = split(v)
      // recursively process each half
      val l = foldMapV(left, m)(f)
      val r = foldMapV(right, m)(f)
      // combine answers with monoid
      m.op(l, r)
    }
  }

  def split[A](v: IndexedSeq[A]): (IndexedSeq[A], IndexedSeq[A]) = {
    val mid: Int = v.length / 2
    (v.slice(0, mid), v.slice(mid, v.length))
  }


  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
      override def zero: Par[A] = Par.unit(m.zero)
    }
  }


  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM: Monoid[Par[B]] = par(m)
    if (v.isEmpty) parM.zero
    else if (v.length == 1) Par.lazyUnit(f(v.head))
    else {
      val (left, right) = split(v)
      val l = parFoldMap(left, m)(f)
      val r = parFoldMap(right, m)(f)
      parM.op(l, r)
    }
  }


  def isOrdered[A](v: IndexedSeq[A])(implicit ordered: Ordered[A]): Boolean = {
    val m: Monoid

  }
}
