package day18

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {
    import Day18._

    "evaluate" should "give the correct answer for example without brackets" in {
        evaluate(Seq(Val(1), Plus, Val(2), Mul, Val(3), Plus, Val(4), Mul, Val(5), Plus, Val(6))) shouldBe 71
    }

    it should "give the correct answer for example with brackets" in {
        evaluate(Seq(Val(2), Mul, Val(3), Plus, LParen, Val(4), Mul, Val(5), RParen)) shouldBe 26
    }

    it should "give the correct answer for example with nested brackets" in {
        // ((1 + 2) * (3 + 4))
        evaluate(
            Seq(LParen, LParen, Val(1), Plus, Val(2), RParen, Mul, LParen, Val(3), Plus, Val(4), RParen, RParen)
        ) shouldBe 21   
    } 

    "tokenize" should "give the correct answer for example" in {
        tokenize("1 + (2 * 3)".iterator) shouldBe Seq(Val(1), Plus, LParen, Val(2), Mul, Val(3), RParen)
    }

    "evaluate" should "give correct answer for longer example" in {
        evaluate(tokenize("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".iterator)) shouldBe 13632
    }
}
