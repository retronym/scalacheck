// Compiled against trunk r429

package applicative

// What is an Applicative Functor?
trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def apply[A, B](f: F[A => B], a: F[A]): F[B]
}

// Some applicative instances
object Applicative {
  implicit val ListApplicative = new Applicative[List] {
    def pure[A](a: A) = List(a)
    def apply[A, B](f: List[A => B], a: List[A]) = for(ff <- f; aa <- a) yield ff(aa)
  }

  implicit val OptionApplicative = new Applicative[Option] {
    def pure[A](a: A) = Some(a)
    def apply[A, B](f: Option[A => B], a: Option[A]) = for(ff <- f; aa <- a) yield ff(aa)
  }

  implicit val Function0Applicative: Applicative[Function0] = new Applicative[Function0] {
    def pure[A](a: A) = new Function0[A] {
      def apply = a
    }
    def apply[A, B](f: Function0[A => B], a: Function0[A]) = new Function0[B] {
      def apply = f()(a())
    }
  }

  // Required to partially apply type constructor arguments
  trait PartialApply1Of2[T[_, _], A] {
    type Apply[B] = T[A, B]
  }

  implicit def Function1Applicative[R] = new Applicative[PartialApply1Of2[Function1, R]#Apply] {
    def pure[A](a: A) = (_: R) => a
    def apply[A, B](f: R => A => B, a: R => A) = (r: R) => f(r)(a(r))
  }
}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

// The four applicative laws; identity, composition, homomorphism, interchange
object ApplicativeLaws {
  def identityLaw[F[_], A](implicit
    p: Applicative[F],
    a: Arbitrary[F[A]]) =
    forAll((k: F[A]) =>
      p.apply(p.pure[A => A](t => t), k) == k)

  def compositionLaw[F[_], A, B, C](implicit
    p: Applicative[F],
    a: Arbitrary[F[A]],
    b: Arbitrary[F[A => B]],
    c: Arbitrary[F[B => C]]) =
    forAll((v: F[B => C], w: F[A => B], x: F[A]) =>
      p.apply(v, p.apply(w, x)) ==
      p.apply(p.apply(p.apply(p.pure((f: B => C) => (g: A => B) => f compose g), v), w), x))

  def homomorphismLaw[F[_], A, B](implicit
    p: Applicative[F],
    a: Arbitrary[A],
    b: Arbitrary[B]) =
    forAll((f: A => B, a: A) =>
      p.apply(p.pure(f), p.pure(a)) ==
      p.pure(f(a)))

  def interchangeLaw[F[_], A, B](implicit
    p: Applicative[F],
    a: Arbitrary[A],
    b: Arbitrary[F[A => B]]) =
    forAll((f: F[A => B], a: A) =>
      p.apply(f, p.pure(a)) ==
      p.apply(p.pure((f: A => B) => f(a)), f))
}

// Let's cook it
object Main {
  def main(args: Array[String]) {
    import ApplicativeLaws._

    // monomorphicise

    // List
    identityLaw[List, Int].check
    // scala.List bug https://lampsvn.epfl.ch/trac/scala/ticket/761
    //compositionLaw[List, Int, String, List[Int]].check
    homomorphismLaw[List, Int, String].check
    interchangeLaw[List, Int, String].check

    // Option
    identityLaw[Option, Int].check
    compositionLaw[Option, Int, String, List[Int]].check
    homomorphismLaw[Option, Int, String].check
    interchangeLaw[Option, Int, String].check
  }
}

/*
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
*/
