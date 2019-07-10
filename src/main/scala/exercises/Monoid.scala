package exercises

import exercises.Par.Par

import scala.language.higherKinds

/**
  *
  * Created by tdm on 2019-07-03.
  */
trait Monoid[A] {

  /** Satisfies identity law. */
  def zero: A

  /** Satisfies associativity law. */
  def combine(a1: A, a2: A): A
}


object Monoids {

  val stringMonoid = new Monoid[String] {
    override val zero = ""
    override def combine(a1: String, a2: String): String = a1 + a2
  }


  // why is this a def?
  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override val zero = Nil
    override def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }


  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override val zero: Int = 0
    override def combine(a1: Int, a2: Int): Int = a1 + a2
  }


  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override val zero: Int = 1
    override def combine(a1: Int, a2: Int): Int = a1 * a2
  }


  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero: Boolean = false
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }


  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero: Boolean = true
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }


  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override val zero: Option[A] = None
    // TODO does this satisfy associativity?
    override def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }


   // 10.3
   def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
     private[this] val id: A => A = (a: A) => a
     override val zero: A => A = id
     // TODO check order
     override def combine(a1: A => A, a2: A => A): A => A = a1.compose(a2)
   }


  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.combine)


  // 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)


  // 10.6
  def foldLeftViaFoldMap[A](as: List[A], z: A)(op: (A, A) => A): A = {

    val m = new Monoid[A] {
      override val zero: A = z
      override def combine(a1: A, a2: A): A = combine(a1, a2)
    }

    foldMap(as, m)((a: A) => a)
  }


  def foldRightViaFoldMap[A](as: List[A], z: A)(op: (A, A) => A): A = {

    val m = new Monoid[A] {
      override val zero: A = z
      override def combine(a1: A, a2: A): A = combine(a1, a2)
    }

    foldMap(as.reverse, m)((a: A) => a)
  }



  // 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty)
      m.zero
    else if (v.length == 1)
      f(v.head)
    else {
      // split in two
      val (left, right) = split(v)
      // recursively process each half
      val l: B = foldMapV(left, m)(f)
      val r: B = foldMapV(right, m)(f)
      // combine answers with monoid
      m.combine(l, r)
    }
  }

  def split[A](v: IndexedSeq[A]): (IndexedSeq[A], IndexedSeq[A]) = {
    val mid: Int = v.length / 2
    (v.slice(0, mid), v.slice(mid, v.length))
  }


  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override val zero: Par[A] = Par.unit(m.zero)
      override def combine(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.combine)
    }
  }


  // should we be concerned about stack safety?
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM: Monoid[Par[B]] = par(m)
    if (v.isEmpty)
      parM.zero
    else if (v.length == 1)
      Par.lazyUnit(f(v.head))
    else {
      val (left, right) = split(v)
      val l: Par[B] = parFoldMap(left, m)(f)
      val r: Par[B] = parFoldMap(right, m)(f)
      parM.combine(l, r)
    }
  }


  // 10.9
  // use foldmap (like map + fold) to check if ordered
  def isOrdered[A](v: IndexedSeq[A])(implicit ordering: Ordering[A]): Boolean = {
    val m: Monoid[Boolean] = new Monoid[Boolean] {
      override val zero: Boolean = true
      override def combine(a: Boolean, b: Boolean): Boolean = a && b
    }

    if (v.size < 2) true
    else {
      val data: IndexedSeq[(A, A)] = v.sliding(2).map {
        case IndexedSeq(a, b) => (a, b)
      }.toArray[(A, A)]

      val compare: (A, A) => Boolean = (a1: A, a2: A) => ordering.compare(a1, a2) >= 0
      foldMapV(data, m)(compare.tupled)
    }
  }

  // 10.10
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override val zero: WC = Stub("")

    override def combine(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) =>
        Stub(s1 + s2)
      case (Stub(s1), Part(left, count, right)) =>
        Part(s1 + left, count, right)
      case (Part(left, count, right), Stub(s2)) =>
        Part(left, count, right + s2)
      case (Part(left1, count1, right1), Part(left2, count2, right2)) =>
        Part(left1, count1 + (if ((right1 + left2).isEmpty) 0 else 1) + count2, right2)
    }
  }

  // 10.11
  def countWords(str: String): Int = {
      // A single character's count. Whitespace does not count,
      // and non-whitespace starts a new Stub.
      def wc(c: Char): WC = {
        if (c.isWhitespace) Part("", 0, "")
        else Stub(c.toString)
      }

      // `unstub(s)` is 0 if `s` is empty, otherwise 1.
      def unstub(s: String): Int = s.length min 1

      foldMapV(str.toIndexedSeq, wcMonoid)(wc) match {
        case Stub(s) => unstub(s)
        case Part(l, w, r) => unstub(l) + w + unstub(r)
      }
  }

  // monoid homomorphism
  // M.op(f(x), f(y)) == f(N.op(x, y))

  // 10.16
  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (ma.zero, mb.zero)

    override def combine(a1: (A, B), a2: (A, B)): (A, B) = (ma.combine(a1._1, a2._1), mb.combine(a1._2, a2._2))
  }


  // 10.17
  def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => mb.zero

    override def combine(a1: A => B, a2: A => B): A => B = (a: A) => mb.combine(a1(a), a2(a))
  }


  def mapMergeMonoid[K,V](mv: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override val zero = Map.empty[K, V]

    override def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, key) =>
        acc.updated(
          key,
          mv.combine(a.getOrElse(key, mv.zero), b.getOrElse(key, mv.zero))
        )
      }
    }
  }

  // 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

  }
}
