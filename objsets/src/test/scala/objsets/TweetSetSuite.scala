package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2: TweetSet = set1.incl(new Tweet("a", "a body", 20))
    val set3: TweetSet = set2.incl(new Tweet("b", "b body", 30))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c: TweetSet = set3.incl(c)
    val set4d: TweetSet = set3.incl(d)
    val set5: TweetSet = set4c.incl(d)
    val set6: TweetSet = set4c.incl(new Tweet("e", "e body", 40))
    val set7: TweetSet = set6.incl(new Tweet("f", "f body", 10))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 1 set has 20 retweets") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 1)
    }
  }

  test("filter: 1 set over 20 retweets") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets > 20)) === 1)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted is e") {
    new TestSets {
      assert(set7.mostRetweeted.user === "e")
    }
  }

  test("most retweeted has 40 retweets") {
    new TestSets {
      assert(set7.mostRetweeted.retweets === 40)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends: TweetList = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user === "a" || trends.head.user === "b")
    }
  }

  test("descending: set7") {
    new TestSets {
      val trends: TweetList = set7.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user === "e")
      assert(trends.head.retweets === 40)
    }
  }
}
