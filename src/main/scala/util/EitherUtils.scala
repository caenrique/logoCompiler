package util

object EitherUtils {

    implicit class EitherListCanSequence[E, A](el: List[Either[E, A]]) {
      def sequence: Either[E, List[A]] = EitherUtils.sequence(el)
    }

    private def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

    private def map2[E, A, EE >: E, B, C](a: Either[E, A])(b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for { a1 <- a; b1 <- b } yield f(a1,b1)

    private def traverse[E,AA,B](es: List[AA])(f: AA => Either[E, B]): Either[E, List[B]] = {
      es match {
        case Nil => Right(Nil)
        case h::t => map2(f(h))(traverse(t)(f))(_ :: _)
      }
    }
}
