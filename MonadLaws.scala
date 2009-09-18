// Compiled against trunk r428

package monad

// What is a Monad?
trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A, B](f: A => M[B], a: M[A]): M[B]
}

// Some monad instances
object Monad {
  implicit val ListMonad = new Monad[List] {
    def pure[A](a: A) = List(a)
    def bind[A, B](f: A => List[B], a: List[A]) = a flatMap f
  }

  implicit val OptionMonad = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def bind[A, B](f: A => Option[B], a: Option[A]) = a flatMap f
  }

  implicit val Function0Monad: Monad[Function0] = new Monad[Function0] {
    def pure[A](a: A) = new Function0[A] {
      def apply = a
    }
    def bind[A, B](f: A => Function0[B], a: Function0[A]) = new Function0[B] {
      def apply = f(a.apply)()
    }
  }

  // Required to partially apply type constructor arguments
  trait PartialApply1Of2[T[_, _], A] {
    type Apply[B] = T[A, B]
  }

  implicit def Function1Monad[R] = new Monad[PartialApply1Of2[Function1, R]#Apply] {
    def pure[A](a: A) = (_: R) => a
    def bind[A, B](f: A => R => B, a: R => A) = (r: R) => f(a(r))(r)
  }
}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

// The three monad laws; left/right identity and associativty
object MonadLaws {
  def leftIdentityLaw[M[_], A, B](implicit
    m: Monad[M],
    a: Arbitrary[A],
    b: Arbitrary[M[B]]) =
    forAll((a: A, f: A => M[B]) =>
      m.bind(f, m.pure(a)) == f(a))

  def rightIdentityLaw[M[_], A](implicit
    m: Monad[M],
    a: Arbitrary[M[A]]) =
    forAll((a: M[A]) =>
      m.bind(m.pure(_: A), a) == a)

  def associativityLaw[M[_], A, B, C](implicit
    m: Monad[M],
    a: Arbitrary[M[A]],
    b: Arbitrary[M[B]],
    c: Arbitrary[M[C]]) =
    forAll((a: M[A], f: A => M[B], g: B => M[C]) =>
      m.bind(g, m.bind(f, a)) == m.bind((t: A) => m.bind(g, f(t)), a))
}

// Let's cook it
object Main {
  def main(args: Array[String]) {
    import MonadLaws._

    // monomorphicise

    // List
    leftIdentityLaw[List, Int, String].check
    rightIdentityLaw[List, Int].check
    associativityLaw[List, Int, String, List[Int]].check

    // Option
    leftIdentityLaw[Option, Int, String].check
    rightIdentityLaw[Option, Int].check
    associativityLaw[Option, Int, String, List[Int]].check
  }
}

