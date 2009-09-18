// Compiled against trunk r429

package monoid

// What is a Monoid?
trait Monoid[A] {
  def append(a1: A, a2: A): A
  def zero: A
}

// Some monoid instances
object Monoid {
  implicit def ListMonoid[A] = new Monoid[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
    def zero = Nil
  }

  implicit def OptionMonoid[A] = new Monoid[Option[A]] {
    def append(a1: Option[A], a2: Option[A]) = if(a1.isDefined) a1 else a2
    def zero = None
  }

  implicit val StringMonoid = new Monoid[String] {
    def append(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  val AddMonoid = new Monoid[Int] {
    def append(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val MultiplyMonoid = new Monoid[Int] {
    def append(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  implicit def Function1Monoid[A, B](implicit mb: Monoid[B]) = new Monoid[A => B] {
    def append(a1: A => B, a2: A => B) = (a: A) => mb.append(a1(a), a2(a))
    def zero = (_: A) => mb.zero
  }
}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

// The three monoid laws; left/right identity and associativity
object MonoidLaws {
  def leftIdentityLaw[A](implicit
    m: Monoid[A],
    a: Arbitrary[A]) =
    forAll((k: A) =>
      m.append(m.zero, k) == k)

  def rightIdentityLaw[A](implicit
    m: Monoid[A],
    a: Arbitrary[A]) =
    forAll((k: A) =>
      m.append(k, m.zero) == k)

  def associativityLaw[A](implicit
    m: Monoid[A],
    a: Arbitrary[A]) =
    forAll((x: A, y: A, z: A) =>
      m.append(m.append(x, y), z) == m.append(x, m.append(y, z)))
}


// Let's cook it
object Main {
  def main(args: Array[String]) {
    import MonoidLaws._

    // monomorphicise

    // List
    leftIdentityLaw[List[Int]].check
    rightIdentityLaw[List[Int]].check
    associativityLaw[List[Int]].check

    // Option
    leftIdentityLaw[Option[Int]].check
    rightIdentityLaw[Option[Int]].check
    associativityLaw[Option[Int]].check

    // Add
    {
      implicit val m = Monoid.AddMonoid

      leftIdentityLaw[Int].check
      rightIdentityLaw[Int].check
      associativityLaw[Int].check
    }

    // Multiply
    {
      implicit val m = Monoid.MultiplyMonoid

      leftIdentityLaw[Int].check
      rightIdentityLaw[Int].check
      associativityLaw[Int].check
    }
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
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
+ OK, passed 100 tests.
*/