package parser

import parser.Monoid.Semigroup

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {

  def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  implicit class MonoidOps[A: Monoid](a: A) {
    def +(b: A) = Monoid[A].combine(a, b)
    def empty = Monoid[A].empty
  }

}

