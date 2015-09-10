package fpinscala.laziness

import org.specs2.mutable.Specification

import scala.{Stream => _}

class StreamTest extends Specification {
  "toList" should {
    "Return a List" in {
      Stream(1, 2, 3, 4, 5).toList must be equalTo List(1, 2, 3, 4, 5)
    }
  }
  "take" should {
    "take the first n items" in {
      Stream(1, 2, 3, 4, 5).take(3).toList must be equalTo List(1, 2, 3)
    }
  }
  "drop" should {
    "drop the first n items" in {
      Stream(1, 2, 3, 4, 5).drop(3).toList must be equalTo List(4, 5)
    }
  }
  "takeWhile" should {
    "take the first items that match p" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 1).toList must be equalTo List(1)
    }
  }
  "takeWhileViaFoldRight" should {
    "take the first items that match p" in {
      Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ % 2 == 1).toList must be equalTo List(1)
    }
  }
  "forAll" should {
    "return true when p is true for all items in the stream" in {
      Stream(1, 2, 3, 4, 5).forAll(_ < 10) must be equalTo true
    }
    "return false when p is not true for all items in the stream" in {
      Stream(1, 2, 3, 4, 5).forAll(_ < 3) must be equalTo false
    }
  }
  "headOption" should {
    "return None when the stream is empty" in {
      Stream().headOption must beNone
    }
    "return some of the head value when the stream has stuff in it" in {
      Stream(1, 2, 3, 4, 5).headOption must beSome(1)
    }
  }
  "headOptionViaFoldRight" should {
    "return None when the stream is empty" in {
      Stream().headOptionViaFoldRight must beNone
    }
    "return some of the head value when the stream has stuff in it" in {
      Stream(1, 2, 3, 4, 5).headOptionViaFoldRight must beSome(1)
    }
  }
  "map" should {
    "apply the function to each value and return a stream" in {
      Stream(1, 2, 3, 4, 5).map(_ * 2).toList must be equalTo List(2, 4, 6, 8, 10)
    }
  }
  "filter" should {
    "apply the predicate to each value and return a stream of values that were true" in {
      Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList must be equalTo List(2, 4)
    }
  }
  "append" should {
    "append one stream to the end of the other" in {
      Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8, 9, 10)).toList must be equalTo List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    }
  }
  "flatmap" should {
    "do stuff" in {
      Stream(1, 2, 3, 4, 5).flatmap(a => Stream(a,a + 1)).toList must be equalTo List(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)
    }
  }
  "constant" should {
    "return the same stuff over and over" in {
      Stream.constant(10).take(10).toList must be equalTo List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
    }
  }
  "from" should {
    "return an ever incrementing list" in {
      Stream.from(10).take(10).toList must be equalTo List(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
    }
  }
  "fibs" should {
    "commit a functional programming clichee" in {
      Stream.fibs.take(10).toList must be equalTo List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }
  "fibsViaUnfold" should {
    "also commit a functional programming clichee" in {
      Stream.fibsUnfold.take(10).toList must be equalTo List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }
  "unfold" should {
    "unfold things" in {
      Stream.unfold(10)(s => Some((s, s+1))).take(10).toList must be equalTo List(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
    }
  }
}
