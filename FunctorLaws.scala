// Compiled against trunk r425

// What is a Functor?
trait Functor[F[_]] {
  def fmap[A, B](f: A => B, value: F[A]): F[B]
}

// Some functor instances
object Functor {
  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](f: A => B, value: List[A]) = value map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B, value: Option[A]) = value map f
  }

  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](f: A => B, value: Function0[A]) = new Function0[B] {
      def apply = f(value.apply)
    }
  }

  // Required to partially apply type constructor arguments
  trait PartialApply1Of2[T[_, _], A] {
    type Apply[B] = T[A, B]
  }

  implicit def Function1Functor[R] = new Functor[PartialApply1Of2[Function1, R]#Apply] {
    def fmap[A, B](r: A => B, f: R => A) = r compose f
  }
}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

// The two functor laws; identity and composition
object FunctorLaws {
  def identityLaw[F[_], A](implicit
    f: Functor[F],
    a: Arbitrary[F[A]]) =
    forAll((k: F[A]) =>
      f.fmap((z: A) => z, k) == k)

  def compositionLaw[F[_], A, X, Y](implicit
    f: Functor[F],
    a: Arbitrary[F[A]],
    x: Arbitrary[X],
    y: Arbitrary[Y]) =
    forAll((k: F[A], t: A => X, u: X => Y) =>
      f.fmap(u, f.fmap(t, k)) == f.fmap(u compose t, k))
}

// Let's cook it
object Main {
  def main(args: Array[String]) {
    import FunctorLaws._

    // monomorphicise

    // List
    identityLaw[List, Int].check
    compositionLaw[List, Int, String, List[Int]].check

    // Option
    identityLaw[Option, Int].check
    compositionLaw[Option, Int, String, List[Int]].check
  }
}

/*
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
*/
