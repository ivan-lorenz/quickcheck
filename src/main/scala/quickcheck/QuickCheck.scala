package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genEmptyHeap: Gen[H] = Gen.const(empty)
  lazy val genNonEmptyHeap: Gen[H] = for {
    i <- arbitrary[A]
    heap <- Gen.oneOf(genEmptyHeap, genNonEmptyHeap)
  } yield insert(i, heap)
  lazy val genHeap: Gen[H] = Gen.oneOf(genEmptyHeap, genNonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (i: A, j: A) =>
    val h = insert(j, insert(i, empty))
    if (j <= i)
      findMin(h) == j
    else
      findMin(h) == i
  }

  property("min3") = forAll { (i: A) =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  property("meld1") = forAll (genNonEmptyHeap, genNonEmptyHeap) {(h: H, g: H) =>
    val f = meld(h,g)
    findMin(f) == findMin(h) || findMin(f) == findMin(g)
  }

  property("meld2") = forAll (genNonEmptyHeap) {(h: H) =>
    val f = meld(h,empty)
    findMin(f) == findMin(h)
  }

  property("meld3") = forAll {(h:H, g:H) =>
    if (isEmpty(h))
      true
    else
      areEqual(meld(h,g), meld(deleteMin(h), insert(findMin(h), g)))
  }

  property("ord1") = forAll {(h: H) =>
    isOrdered(h)
  }

  private def isOrdered(h: H) = {
    def loop(i: A, j:H): Boolean = {
      if (isEmpty(j))
        true
      else {
        val min = findMin(j)
        ord.lteq(i, min) && loop(min, deleteMin(j))
      }
    }
      if (isEmpty(h))
        true
      else
        loop(findMin(h), deleteMin(h))
  }

  private def areEqual(k:H, f:H) : Boolean = {
    if (isEmpty(meld(k,f))) true
    else {
      val i = findMin(k)
      val j = findMin(f)
      i == j && areEqual(deleteMin(k), deleteMin(f))
    }
  }

}
