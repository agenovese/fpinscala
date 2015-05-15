package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)


  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case Some(a) if !f(a) => None
    case None => None
  }

  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None => None
  }

  def filter3(p: A => Boolean): Option[A] =
    flatMap(a => if(p(a)) Some(a) else None)
  }

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap {m => mean(xs map {x => Math.pow(x - m, 2)})}


//  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
//    oa flatMap {a => ob map {b => f(a,b)}}

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield {
      f(a,b)
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case Some(h) :: t => sequence(t) map (h :: _)
      case None :: _ => None
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()): Option[List[B]])({
      (a,ol) => // f(a) flatMap {b => ol map {b :: _}}
        for {
          b <- f(a)
          l <- ol
        } yield {
          b :: l
        }
    })
}