package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    (a < b) ==> {
      val h = insert(a, insert(b, empty))
      findMin(h) == a
    }
  }

  property("min3") = forAll { (a: Int, b: Int) =>
    (a > b) ==> {
      val h = insert(a, insert(b, empty))
      findMin(h) == b
    }
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("meld1") = forAll { h: H =>
    meld(h, empty) == h
  }

  property("meld2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)

    meld(h, i) == insert(a, insert(b, empty))
  }

  property("meld3") = forAll { (i: H, j: H) =>
    val minI = findMin(i)
    val minJ = findMin(j)
    val melded = meld(i, j)

    findMin(melded) == Math.min(minI, minJ)
  }

  property("meld4") = forAll { (i: H, j: H) =>
    @tailrec
    def heapEqual(i: H, j: H): Boolean = {
      isEmpty(i) && isEmpty(j) || {
        val minI = findMin(i)
        val minJ = findMin(j)
        minI == minJ && heapEqual(deleteMin(i), deleteMin(j))
      }
    }

    val meldedOneWay = meld(i, j)
    val meldedAnotherWay = meld(deleteMin(i), insert(findMin(i), j))

    heapEqual(meldedOneWay, meldedAnotherWay)
  }

  property("sorted") = forAll { (h: H) =>
    @tailrec
    def sorted(lastMin: Int, heap: H): Boolean = {
      isEmpty(heap) || {
        val min = findMin(heap)
        lastMin <= min && sorted(min, deleteMin(heap))
      }
    }

    isEmpty(h) || sorted(findMin(h), deleteMin(h))
  }
}
