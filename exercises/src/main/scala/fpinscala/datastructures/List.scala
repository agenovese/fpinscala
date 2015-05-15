package fpinscala.datastructures

sealed trait List[+A] {
  def flatMap[B](f: A => List[B]): List[B] =
    List.foldRight(this, List(): List[B])((a, bs) => List.append(f(a), bs))

  def map[B](f: A => B): List[B] = List.foldRight(this, List(): List[B])((a, bs) => Cons(f(a), bs))

}

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/*
Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
*/
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, ls) => Cons(a, ls))

  def concat[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, List(): List[A])((s, d) => appendViaFoldRight(s, d))

  def concat2[A](l: List[List[A]]): List[A] =
    l match {
      case Cons(h, t) => appendViaFoldRight(h, concat(t))
      case Nil => Nil
    }

  def add1(l: List[Int]): List[Int] = foldRight(l, List(): List[Int])((i, ls) => Cons(i + 1, ls))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, List(): List[String])((i, ls) => Cons(i.toString, ls))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List(): List[B])((a, bs) => Cons(f(a), bs))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, List(): List[A])((a, bs) => if (p(a)) Cons(a, bs) else bs)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, List(): List[B])((a, bs) => append(f(a), bs))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((a) => if(f(a)) List(a) else List())

  //pad to longest
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Cons(ha,ta), Cons(hb,tb)) => Cons(ha + hb, addPairwise(ta, tb))
    case (Nil, b) => b
    case (h, Nil) => h
  }

  //truncate to shortest
  def addPairwise2(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Cons(ha,ta), Cons(hb,tb)) => Cons(ha + hb, addPairwise(ta, tb))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Cons(ha,ta), Cons(hb,tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    length(l) >= length(prefix) && foldRight(zipWith(l,prefix)((a, p) => a == p), true)((a,b) => a && b)

  def startsWith2[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (Cons(hl,tl), Cons(hp,tp)) => hl == hp && startsWith(tl, tp)
    case (Nil, Cons(_, _)) => false
    case (_, Nil) => true
  }


  //  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    if(length(l) >= length(sub)) {
      startsWith(l, sub) || hasSubsequence(tail(l), sub)
    } else {
      false
    }
}
