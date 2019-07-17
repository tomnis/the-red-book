package exercises

/**
  *
  * Created by tdm on 2019-07-16.
  */
sealed trait Validation[+E, +A]

case class Invalid[E](head: E, tail: Vector[E] = Vector.empty)
extends Validation[E, Nothing]

case class Valid[A](a: A) extends Validation[Nothing, A]


object Validation {
  def apply[A](a: A) = Valid(a)
}