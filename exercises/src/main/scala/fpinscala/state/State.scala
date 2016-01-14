package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  case class Fake(value: Int) extends RNG {
    def nextInt: (Int, RNG) = (value, this)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = {
    rng => rng.nextInt
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (i >>> 1, rng2)
  }

  def doubleRand: Rand[Double] = map(int) {
    i => (i >>> 1).toDouble / (Int.MaxValue.toDouble + 1.0)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1.0), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, rng2) = intDouble(rng)
    ((t._2, t._1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i1, r1) = rng.nextInt
      val rest = ints(count - 1)(r1)
      (i1 :: rest._1, rest._2)
    } else {
      (Nil, rng)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDoubleWithMap2: Rand[(Int, Double)] =
    map2(int, doubleRand)((a, b) => (a, b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case ra :: t =>
          val (a, r2) = ra(rng)
          val (l, r3) = sequence(t)(r2)
          (a :: l, r3)
        case Nil => (Nil, rng)
      }
    }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      val rb: Rand[B] = g(a)
      rb(r2)
    }

  def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {

    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for{
      _ <- sequence(inputs.map(inputToState))
      m <- get
    } yield (m.candies, m.coins)


  def inputToState(input: Input): State[Machine, Unit] =
    State( m =>
      (input, m) match {
        case (Coin, Machine(true, candy, coins)) if candy > 0 => ((), Machine(locked = false, candy, coins + 1))
        case (Turn, Machine(false, candy, coins)) => ((), Machine(locked = true, candy - 1, coins))
        case (Turn, Machine(true, _, _)) => ((), m)
        case (Coin, Machine(false, _, _)) => ((), m)
        case (_, Machine(_, candy, _)) if candy == 0 => ((), m)
      }
    )

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => {
      fs match {
        case ra :: t =>
          val (a, s2) = ra.run(s)
          val (l, s3) = sequence(t).run(s2)
          (a :: l, s3)
        case Nil => (Nil, s)
      }
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}


