package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or
// another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val result = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def drop2[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n <= 0 => t
    case Cons(_, t) if n > 0 => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, (Cons(_, Nil))) => Cons(x, Nil)
    case Cons(x, tail) => Cons(x, init(tail))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, c) => c + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFoldLeft(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((c, _) => c + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((l, a) => Cons(a, l))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a,b))

//  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))


  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = ???

  def concat[A](l: List[List[A]]): List[A] = ???

  def add1(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A, B](l: List[A])(f: A => B): List[B] = ???

  def filter[A](l: List[A])(f: A => Boolean): List[A] = ???

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = ???

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = ???

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = ???

  //  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = ???

}