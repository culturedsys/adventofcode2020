package day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7Spec extends AnyFlatSpec with Matchers {
    import Day7._

    val example = Map(
        Bag("light", "red") -> 
            Seq((1, Bag("bright", "white")), (2, Bag("muted", "yellow"))),
        Bag("dark", "orange") -> 
            Seq((3, Bag("bright", "white")), (4, Bag("muted", "yellow"))),
        Bag("bright", "white") ->
            Seq((1, Bag("shiny", "gold"))),
        Bag("muted", "yellow") ->
            Seq((2, Bag("shiny", "gold")), (9, Bag("faded", "blue"))),
        Bag("shiny", "gold") ->
            Seq((1, Bag("dark", "olive")), (2, Bag("vibrant", "plum"))),
        Bag("dark", "olive") ->
            Seq((3, Bag("faded", "blue")), (4, Bag("dotted", "black"))),
        Bag("vibrant", "plum") ->
            Seq((5, Bag("faded", "blue")), (6, Bag("dotted", "black"))),
        Bag("faded", "blue") -> Seq(),
        Bag("dotted", "black") -> Seq()
    )

    "countBagsContaining" should "find the correct answer from example" in {
        countBagsContaining(Bag("shiny", "gold"), example) shouldBe 4
    }

    "countBagsContainedIn" should "find the correct answer from example" in {
        countBagsContainedIn(Bag("shiny", "gold"), example) shouldBe 32
    }

    "parse" should "parse example with multiple entries" in {
        parse("light red bags contain 1 bright white bag, 2 muted yellow bags.") shouldBe
            Right((Bag("light", "red") -> Seq((1, Bag("bright", "white")), (2, Bag("muted", "yellow")))))
    }
}
