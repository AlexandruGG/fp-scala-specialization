package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t0 = Leaf('a', 6)
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val l0 = List('a', 'b', 'a')
    val l1 = List('a', 'b', 'c', 'c', 'd', 'a', 'a')
    val s0 = List(t2)
    val s1 = List(t2, t1)
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(t0) === 6)
    }
  }

  test("weight of a smaller tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(t0) === List('a'))
    }
  }

  test("chars of a smaller tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times with smaller list") {
    new TestTrees {
      assert(times(l0) === List(('b', 1), ('a', 2)))
    }
  }

  test("times with larger list") {
    new TestTrees {
      assert(times(l1) === List(('b', 1), ('d', 1), ('a', 3), ('c', 2)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton with one single code tree") {
    new TestTrees {
      assertResult(true)(singleton(s0))
    }
  }

  test("singleton with multiple code trees") {
    new TestTrees {
      assertResult(false)(singleton(s1))
    }
  }

  test("combine of some leaf list") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leafList) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("createCodeTree with smaller list") {
    new TestTrees {
      assertResult(Fork(Leaf('b', 1), Leaf('a', 2), List('b', 'a'), 3))(createCodeTree(l0))
    }
  }

  test("createCodeTree with larger list") {
    new TestTrees {
      assertResult(
        Fork(Fork(Fork(Leaf('b', 1), Leaf('d', 1), List('b', 'd'), 2), Leaf('c', 2), List('b', 'd', 'c'), 4), Leaf('a', 3), List('b', 'd', 'c', 'a'), 7)
      )(createCodeTree(l1))
    }
  }

  test("decode with smaller tree") {
    new TestTrees {
      assertResult(List('b', 'a', 'a'))(decode(t1, List(1, 0, 0)))
    }
  }

  test("decodedSecret") {
    new TestTrees {
      assertResult(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))(decode(frenchCode, secret))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode with smaller tree") {
    new TestTrees {
      assertResult(List(1, 0, 0))(encode(t1)(List('b', 'a', 'a')))
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode with smaller tree") {
    new TestTrees {
      assertResult(List(1, 0, 0))(quickEncode(t1)(List('b', 'a', 'a')))
    }
  }

}
