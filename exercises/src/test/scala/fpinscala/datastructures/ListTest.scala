package fpinscala.datastructures

import org.specs2.mutable.Specification

import scala.{List => _}
import fpinscala.datastructures.List._

class ListTest extends Specification {
  "Calling tail on a list" should {
    "Return a List of all the elements in the input except the first" in {
      tail(List(1, 2, 3, 4, 5)) must be equalTo List(2, 3, 4, 5)
    }
    "Return Nil in the case of a single item list" in {
      tail(List(1)) must be equalTo Nil
    }
  }
}
