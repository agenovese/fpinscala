package fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeTest extends Specification {
  val tree = Branch(Leaf(1), Leaf(2))
  val deepTree = Branch(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Leaf(2)), Leaf(2))
  "size" should {
    "Return the total number of nodes in the tree" in {
      Tree.size(tree) must be equalTo 3
    }
  }
  "maximum" should {
    "Return the largest integer in the tree" in {
      Tree.maximum(tree) must be equalTo 2
    }
  }
  "depth" should {
    "Return the maximum path length from the root of a tree to any leaf" in {
      Tree.depth(tree) must be equalTo 2
    }
    "Return the maximum path length from the root of a deep tree to any leaf" in {
      Tree.depth(deepTree) must be equalTo 5
    }
  }
  "map" should {
    "transform the contents of the tree using the given function without altering its structure" in {
      Tree.map(tree)(_.toString) must be equalTo Branch(Leaf("1"), Leaf("2"))
    }
  }
  "fold" should {
    "allow implementing size" in {
      Tree.sizeViaFold(tree) must be equalTo 3
    }
    "allow implementing maximum" in {
      Tree.maximumViaFold(tree) must be equalTo 2
    }
    "allow implementing depth" in {
      Tree.depthViaFold(tree) must be equalTo 2
      Tree.depthViaFold(deepTree) must be equalTo 5
    }
    "allow implementing map" in {
      Tree.mapViaFold(tree)(_.toString) must be equalTo Branch(Leaf("1"), Leaf("2"))
    }
  }
}
