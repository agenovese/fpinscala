package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = { this.foldRight(List(): List[A]) { (a, l) => a :: l } }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (n > 0) Cons(h, () => t().take(n - 1))
      else Empty
  }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) { s => s match {
      case (Empty, _) => None
      case (Cons(h, t), i: Int) if i == 0 => None
      case (Cons(h, t), i: Int) if i != 0 => Some((h(), (t(), n - 1)))
    }}


  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) =>
      if (n > 1) t().drop(n - 1)
      else t()
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream(): Stream[A]){(a,s) =>
      if (p(a)) Cons(() => a, () => s)
      else Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
      }
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) { s => s match {
      case Empty => None
      case Cons(h, t) if !p(h()) => None
      case Cons(h, t) if p(h()) => Some((h(), t()))
    }}



  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) =>
      if (p(h())) t().forAll(p)
      else false
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A]){(a,o) => Some(a) }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]){(a, o) => cons(f(a), o)}

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) { s =>
      for{
        a <- s.headOption
        t <- s.tailOption
      } yield {
        (f(a), t)
      }
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]){(a, s) =>
      if (p(a)) cons(a, s)
      else s
    }

  def append[B >: A](bs: Stream[B]): Stream[B] =
    foldRight(bs){(a, bs) => cons(a,bs)}

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]){(a, bs) => f(a).append(bs)}

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, other)) { s =>
      for {
        a <- s._1.headOption
        b <- s._2.headOption
        ta <- s._1.tailOption
        tb <- s._2.tailOption
      } yield {
        (f(a,b), (ta, tb))
      }
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
  unfold((this, other)) { s => s match {
    case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
    case (Cons(ha, ta), Empty) => Some(((Some(ha()), None), (ta(), Empty)))
    case (Empty, Cons(hb, tb)) => Some(((None, Some(hb())), (Empty, tb())))
    case (Empty, Empty) => None
  }

  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  private def fib_helper(a: Int, b: Int): Stream[Int] = cons(a, fib_helper(b, a + b))

  val fibs = fib_helper(0, 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  def onesUnfold: Stream[Int] = unfold(1)(s => Some((s, s)))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))
  val fibsUnfold: Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))
}