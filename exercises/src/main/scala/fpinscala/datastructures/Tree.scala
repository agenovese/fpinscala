package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def Tree[A](a: A): Tree[A] = Leaf(a)
  def Tree[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l,r)

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Tree(f(v))
    case Branch(l, r) => Tree(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maximumViaFold(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)
  def depthViaFold[A](t: Tree[A]): Int = fold(t)(v => 1)((l,r) => 1 + (l max r))
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Tree(f(v)))((l,r) => Tree(l,r))
}