package day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day2Spec extends AnyFlatSpec with should.Matchers {
    import Day2._

    "isInRange" should "return true if valid" in {
        isInRange(Rule(1, 3, 'a'), "abcde") shouldBe true
    } 

    "isInRange" should "return false if invalid" in {
        isInRange(Rule(1, 3, 'b'), "cdefg") shouldBe false
    } 

    "parse" should "parse correctly" in {
        parse("1-3 a: abcde") shouldBe Some((Rule(1, 3, 'a'), "abcde"))
    }

    "countValid" should "count the example correctly" in {
        val example = Seq(
            "1-3 a: abcde",
            "1-3 b: cdefg",
            "2-9 c: ccccccccc"
        )

        countValid(example, isInRange) shouldBe Some(2)
    }

    "isAtIndex" should "return true if valid" in {
        isAtIndex(Rule(1, 3, 'a'), "abcde") shouldBe true
    }

    "isAtIndex" should "return false if invalid" in {
        isAtIndex(Rule(1, 3, 'b'), "cdefg") shouldBe false
    }
}
