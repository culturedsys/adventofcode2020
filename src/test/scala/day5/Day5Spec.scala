package day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
    import Day5._

    "binaryPartition" should "give the correct answer for example" in { 
        binaryPartition(Seq(Lower, Upper, Lower, Upper, Upper, Lower, Lower), 0 to 127) shouldBe 44
    }

    "parse" should "correctly parse example" in {
        parse("FBFBBFFRLR") shouldBe Some((Seq(Lower, Upper, Lower, Upper, Upper, Lower, Lower, Upper, Lower, Upper)))
    }

    "largestSeatId" should "give the correct answer for example" in {
        calculateSeatIds(Seq(
            "BFFFBBFRRR",
            "FFFBBBFRRR",
            "BBFFBBFRLL"
        ).iterator).flatMap(largestSeatId) shouldBe Some(820)
    }
}

