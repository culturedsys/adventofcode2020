package day11

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
    import Day11._

    "parse" should "give the correct result" in {
        parse(Seq(".#.", 
              "#L#",
              "L#.").iterator) shouldBe Field(3, 3, 
                Vector(Floor, Occupied, Floor, 
                    Occupied, Empty, Occupied,
                    Empty, Occupied, Floor))
    }

    "countAdjacent" should "give the correct result" in {
        parse(
            Seq(".#.",
                "###",
                ".#."
        ).iterator).countAdjacent(1, 1) shouldBe 4
    }

    "evolveState" should "give the correct result for occupied with adjacentRule" in {
        val before = parse(
            Seq(".#.",
                "###",
                ".#.").iterator)
        val after = parse(
            Seq(".#.",
                "#L#",
                ".#.").iterator)

        before.evolveState(_.adjacentRule) shouldBe after
    } 

    "evolveState" should "give the correct result for empty with adjacentRule" in {
        val before = parse(
            Seq(".L.",
                "LLL",
                ".L.").iterator)
        val after = parse(
            Seq(".#.",
                "###",
                ".#.").iterator)

        before.evolveState(_.adjacentRule) shouldBe after
    } 

    "findFirstOnVector" should "give the correct result for blocked" in {
        val field = parse(
            Seq("...",
                ".#.",
                "..L").iterator
        )

        field.findFirstOnVector(0, 0, 1, 1) shouldBe Some(Occupied)
    }

    "findFirstOnVector" should "give the correct result for not blocked" in {
        val field = parse(
            Seq("...",
                "...",
                "..L").iterator
        )

        field.findFirstOnVector(0, 0, 1, 1) shouldBe Some(Empty)
    }

    "countVisible" should "give the correct result" in {
        val field = parse(
            Seq("...",
                "#..",
                ".##").iterator
        )

        field.countVisible(1, 1) shouldBe 3
    }
}
