package fpinscala.state

import org.specs2.mutable.Specification

/**
  * Created by angelo on 2015-12-09.
  */
class RngTest extends Specification {
  "nonNegativeInt" should {
    "Return a non negative Integer" in {
      (RNG.nonNegativeInt(RNG.Fake(0))._1 > -1) should beTrue
    }
    "Return a non negative Integer from a negative result" in {
      (RNG.nonNegativeInt(RNG.Fake(Int.MinValue))._1 > -1) should beTrue
    }
    "Return a double bigger than 0.0" in {
      (RNG.double(RNG.Fake(-12345))._1 >= 0.0) should beTrue
    }
    "Return a double less than 1.0" in {
      (RNG.double(RNG.Fake(Int.MaxValue))._1 < 1.0) should beTrue
    }
    "Return a different result on subsequent calls" in {
      val (v1, rng) = RNG.double(RNG.Simple(0L))
      val (v2, _) = RNG.double(rng)

      v1 != v2 should beTrue
    }
    "Return a list of ints" in {
      val (ints, _) = RNG.ints(10)(RNG.Simple(0))
      ints.groupBy(i => i) should haveSize(10)
    }
    "Also return a list of ints" in {
      val (ints, _) = RNG.intsViaSequence(10)(RNG.Simple(0))
      ints.groupBy(i => i) should haveSize(10)
    }
//    "do intDouble with map2" in {
//      val ((i: Int, d: Double), rng: RNG) = RNG.intDoubleWithMap2(RNG.Simple(44L))
//
//    }
  }

}
