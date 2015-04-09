package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.specs2.mutable.Specification

import scala.{List => _}

class ListTest extends Specification {
  "tail" should {
    "Return a List of all the elements in the input except the first" in {
      tail(List(1, 2, 3, 4, 5)) must be equalTo List(2, 3, 4, 5)
    }
    "Return Nil in the case of a single item list" in {
      tail(List(1)) must be equalTo Nil
    }
    "Return Nil in the case of Nil input" in {
      tail(Nil) must be equalTo Nil
    }
  }
  "setHead" should {
    "Return a List of all the elements in the input with the first replaced" in {
      setHead(List(1, 2, 3, 4), 5) must be equalTo List(5, 2, 3, 4)
    }
    "Return a list with the single item replaced in the case of a single item list" in {
      setHead(List(1), 5) must be equalTo List(5)
    }
    "Do what in the case of an empty list?" in {
      setHead(Nil, 5) must be equalTo List(5)
    }
  }
  "drop" should {
    "Return the original list when n is 0" in {
      drop(List(1, 2, 3, 4, 5), 0) must be equalTo List(1, 2, 3, 4, 5)
    }
    "Return a List of all the elements in the input with the first n removed" in {
      drop(List(1, 2, 3, 4, 5), 2) must be equalTo List(3, 4, 5)
    }
    "Return an empty list when all the items in the list are dropped" in {
      drop(List(1), 1) must be equalTo Nil
    }
    "Do what when more items are dropped than are in the list" in {
      drop(List(1), 2) must be equalTo Nil
    }
    "Do what in the case of an empty list?" in {
      drop(Nil, 0) must be equalTo Nil
    }
    "Do what in the case of an empty list?" in {
      drop(Nil, 1) must be equalTo Nil
    }
  }
  "dropWhile" should {
    "Return the original list if the first item doesn't match" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => false) must be equalTo List(1, 2, 3, 4, 5)
    }
    "Return Nil if the all items match" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => true) must be equalTo Nil
    }
    "Return a List of all the elements in the after the first non-match (inclusive)" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i <= 2) must be equalTo List(3, 4, 5)
    }
  }
  "dropWhile" should {
    "Return the original list if the first item doesn't match" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => false) must be equalTo List(1, 2, 3, 4, 5)
    }
    "Return Nil if the all items match" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => true) must be equalTo Nil
    }
    "Return a List of all the elements in the after the first non-match (inclusive)" in {
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i <= 2) must be equalTo List(3, 4, 5)
    }
  }
  "init" should {
    "Drop the last item of a list" in {
      init(List(1, 2, 3, 4, 5)) must be equalTo List(1, 2, 3, 4)
    }
  }
  "append" should {
    "append 2 lists together" in {
      appendViaFoldRight(List(1, 2, 3, 4, 5), List(6, 7, 8, 9)) must be equalTo List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }
  "Concat" should {
    "concatenate 3 lists together" in {
      concat(List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9), List(10, 11, 12, 13))) must be equalTo
        List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    }
  }
  "add1" should {
    "add 1 to each element in the list" in {
      add1(List(1, 2, 3, 4, 5)) must be equalTo List(2, 3, 4, 5, 6)
    }
  }
  "doubleToString" should {
    "convert list of doubles to list of strings" in {
      doubleToString(List(1, 2, 3, 4, 5)) must be equalTo List("1.0", "2.0", "3.0", "4.0", "5.0")
    }
  }
  "filter" should {
    "remove all odd numbers from a list of Ints when passed the appropriate predicate" in {
      filter(List(1, 2, 3, 4, 5))(i => i % 2 == 0) must be equalTo List(2,4)
    }
  }
  "flatmap" should {
    "concatenate the results of running f on each item in the list" in {
      flatMap(List(1,2,3))(i => List(i,i)) must be equalTo List(1,1,2,2,3,3)
    }
  }
  "filterViaFlatmap" should {
    "remove all odd numbers from a list of Ints when passed the appropriate predicate" in {
      filterViaFlatMap(List(1, 2, 3, 4, 5))(i => i % 2 == 0) must be equalTo List(2,4)
    }
  }
  "addPairWise" should {
    "add corresponding values in 2 lists" in {
      addPairwise(List(1,2,3), List(4,5,6)) must be equalTo List(5,7,9)
    }
    "do something if the lists differ in length" in {
      addPairwise(List(1,2,3), List(4,5,6,7)) must be equalTo List(5,7,9)
    }
  }
}
