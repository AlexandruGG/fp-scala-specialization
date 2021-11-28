package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  trait TestSets {
    val s1: Set = singletonSet(1)
    val s2: Set = singletonSet(2)
    val s3: Set = singletonSet(3)
    val s4: Set = singletonSet(1000)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Singleton2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s: Set = union(s1, s2)

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements found in both sets") {
    new TestSets {
      val s: Set = union(s1, s2)
      val t: Set = intersect(s1, s)

      assert(contains(t, 1), "Intersect 1")
      assert(!contains(t, 2), "Intersect 2")
      assert(!contains(t, 3), "Intersect 3")
    }
  }

  test("diff contains only elements from the first set which are not found in the second set") {
    new TestSets {
      val s: Set = union(s1, s2)
      val t: Set = diff(s, s2)

      assert(contains(t, 1), "Diff 1")
      assert(!contains(t, 2), "Diff 2")
      assert(!contains(t, 3), "Intersect 3")
    }
  }

  test("filter returns elements for which the predicate holds") {
    new TestSets {
      val s: Set = union(s2, s3)
      val p: Int => Boolean = elem => elem > 2
      val t: Set = filter(s, p)

      assert(contains(t, 3), "Filter 1")
      assert(!contains(t, 2), "Filter 2")
    }
  }

  test("forall returns whether all bounded integers within `s` satisfy `p` or `q`") {
    new TestSets {
      val s: Set = union(s2, s3)
      val p: Int => Boolean = elem => elem > 2
      val q: Int => Boolean = elem => elem > 1


      assert(!forall(s, p), "Forall 1")
      assert(forall(s, q), "Forall 2")
    }
  }

  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      val s: Set = union(s1, s2)
      val p: Int => Boolean = elem => elem > 1
      val q: Int => Boolean = elem => elem > 5

      assert(exists(s, p), "Exists 1")
      assert(!exists(s, q), "Exists 2")
    }
  }

  test("map returns a set transformed by applying `f` to each element of `s`") {
    new TestSets {
      val s: Set = union(s1, s2)
      val f: Int => Int = elem => elem + 2
      val m: Set = map(s, f)

      assert(!contains(m, 1), "Map 1")
      assert(!contains(m, 2), "Map 2")
      assert(contains(m, 3), "Map 3")
      assert(contains(m, 4), "Map 4")
    }
  }
}
