import org.scalacheck._
import Arbitrary.arbitrary
import Gen.{choose, value, oneOf}
import Prop._

trait Tree {
  def size: Int
  def contains(item: Int): Boolean
  def invariant: Boolean
  def add(item: Int): Tree
}

case class Node(left: Tree, right: Tree, item: Int) extends Tree {
  def size = 1 + left.size + right.size
  def contains(it: Int) = it == item || left.contains(it) || right.contains(it)

  def invariant = ((left,right) match {
    case (Node(l1,r1,it1), Node(l2,r2,it2)) => it1 <= item && item <= it2
    case _ => true
  }) && (Math.abs(left.size-right.size) <= 1) && left.invariant && right.invariant

  def add(it: Int) = 
    if(it < item) Node(left.add(it), right, item)
    else Node(left, right.add(it), item)
}

case object Leaf extends Tree {
  def size = 0
  def contains(item: Int) = false
  def invariant = true
  def add(item: Int) = Node(Leaf, Leaf, item)
}

object TreeSpecification extends Properties("Tree") {
  val genTree: Gen[Tree] = for {
    sz <- Gen.choose(0,10)
    item <- arbitrary[Int]
    t <- genNode(item, sz, true)
  } yield t

  def genNode(n: Int, sz: Int, left: Boolean): Gen[Tree] =
    if(sz == 0) value(Leaf)
    else for {
      m <- if(left) choose(n-100, n) else choose(n, n+100)
      b <- oneOf(true, false)
      t1 <- genNode(m, (sz-1)/2, b)
      t2 <- genNode(m, sz - 1 - ((sz-1)/2), !b)
    } yield Node(if(b) t1 else t2, if(b) t2 else t1, m)

  property("genTree") = forAll(genTree) { t => t.invariant }

  property("add") = forAll(genTree, arbitrary[Int]) { (t,n) =>
    val t2 = t.add(n)     
    t2.contains(n) && t2.size == (t.size+1) && t2.invariant :| ("t2: "+t2)
  }
}

TreeSpecification.check
