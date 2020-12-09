package day9

 import org.scalatest.flatspec.AnyFlatSpec
 import org.scalatest.matchers.should.Matchers
 import cats.data.Chain
 
class Day9Spec extends AnyFlatSpec with Matchers {
    import Day9._

    "isNumberValid" should "give correct answers for example" in {
        isNumberValid(1L to 25L, 26L) shouldBe true
        isNumberValid(1L to 25L, 49L) shouldBe true
        isNumberValid(1L to 25L, 100L) shouldBe false
        isNumberValid(1L to 25L, 50L) shouldBe false
    }

    val example = Seq(
        35,
        20,
        15,
        25,
        47,
        40,
        62,
        55,
        65,
        95,
        102,
        117,
        150,
        182,
        127,
        219,
        299,
        277,
        309,
        576
    ).map(_.toLong)

    "findFirstInvalid" should "give correct answers for example" in {
        findFirstInvalid(example, 5) shouldBe Some(127L)
    }

    "findSequenceSummingTo" should "give correct answer for example" in {
        findSequenceSummingTo(example, 127L) shouldBe Some(Seq(15L, 25L, 47L, 40L))
    }
}
