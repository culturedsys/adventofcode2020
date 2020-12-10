package day10

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {
    import Day10._ 

    val example = Seq(
        16,
        10,
        15,
        5,
        1,
        11,
        7,
        19,
        6,
        12,
        4
    )

    "countGaps" should "give the correct answer for example" in {

        countGaps(example) shouldBe Map(1 -> 7, 3 -> 4)
    }

    "countPaths" should "give the correct answer for example" in {
        countPaths(example) shouldBe 8
    }

    val longerExample = Seq(
        28,
        33,
        18,
        42,
        31,
        14,
        46,
        20,
        48,
        47,
        24,
        23,
        49,
        45,
        19,
        38,
        39,
        11,
        1,
        32,
        25,
        35,
        8,
        17,
        7,
        9,
        4,
        2,
        34,
        10,
        3        
    )

    "countPaths" should "give the correct answer for longer example" in {
        countPaths(longerExample) shouldBe 19208
    }
}
