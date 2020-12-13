package day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {
    import Day13._

    "lowestMultipleGreaterThan" should "give the correct answer" in {
        lowestMultipleGreaterThan(7, 939) shouldBe 945
        lowestMultipleGreaterThan(13, 939) shouldBe 949
        lowestMultipleGreaterThan(59, 939) shouldBe 944
    }

    "findMatching" should "give the correct answer" in {
        findMatching(Seq(Some(17),None, Some(13), Some(19)).zipWithIndex) shouldBe 3417
        findMatching(Seq(1789,37,47,1889).map(Some[Int]).zipWithIndex) shouldBe 1_202_161_486
    }
}
