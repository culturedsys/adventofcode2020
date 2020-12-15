package day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Spec extends AnyFlatSpec with Matchers {
    import Day15._

    "iterateResults" should "give correct answer for example" in {
        iterateResults((Map(0 -> 0, 3 -> 1), 6, 2)).take(8).map(_._2) shouldBe Seq(
            6, 0, 3, 3, 1, 0, 4, 0
        )
    }

    "stateFromNumbers" should "give correct answer for example" in {
        stateFromNumbers(Seq(0, 3, 6)) shouldBe (Map(0 -> 0, 3 -> 1), 6, 2)
    }

    "nthResult" should "give correct answer for example" in {
        nthResult(Seq(0, 3, 6), 2020) shouldBe 436
    }
}
