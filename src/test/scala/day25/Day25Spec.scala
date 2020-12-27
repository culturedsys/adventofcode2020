package day25

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day25Spec extends AnyFlatSpec with Matchers {
    import Day25._

    "findLoopSize" should "find loop size for example" in {
        findLoopSize(7, 20201227, 5764801) shouldBe 8
    }

    "findLoopSize" should "find loop size for second example" in {
        findLoopSize(7, 20201227, 17807724) shouldBe 11
    }

    "deriveKey" should "find public key for example" in {
        deriveKey(7, 20201227, 8) shouldBe 5764801
    }
}
