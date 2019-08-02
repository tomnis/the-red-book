package exercises

/**
  * Created by tomas.mccandless on 2019-07-22.
  */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traverse[G, G[A], A](fga)((ga: G[A]) => ga)

  type Id[A] = A

  def idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = {
      f(fa)
    }
  }

  override def foldLeft[A, B](as: F[A])(zero: B)(f: (B, A) => B): B = ???

  // 12.14
  def mapViaTraverse[A,B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idMonad)
  }


  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monads.stateMonad)
  }


//  def reverse[A](fa: F[A]): F[A] = {
//    this.fold
//
//  }
}

object Traverse {

  // 12.13
  val listTraverse = new Traverse[List] {
    /**
      * map(x)(a => a) = x
      *
      * @param fa
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }


  val optionTraverse = new Traverse[Option] {
    /**
      * map(x)(a => a) = x
      *
      * @param fa
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  case class Tree[+A](head: A, tail: List[Tree[A]]) {
    def map[B](f: A => B): Tree[B] = {
      Tree(f(this.head), this.tail.map(t => t.map(f)))
    }
  }

  def treeTraverse[A]: Traverse[Tree] = new Traverse[Tree[A]] {
    /**
      * map(x)(a => a) = x
      *
      * @param fa
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa map f
  }
}