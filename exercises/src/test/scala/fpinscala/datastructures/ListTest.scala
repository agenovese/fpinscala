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
}
