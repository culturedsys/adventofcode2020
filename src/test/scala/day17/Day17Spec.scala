package day17

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Spec extends AnyFlatSpec with Matchers {
    import Day17._

    val example = 
        """.#.
          |..#
          |###""".stripMargin

    "parse" should "parse a map" in {
        parse(example.linesIterator) shouldBe Set(
                  Seq(1, 0, 0),
                  Seq(2, 1, 0),
                  Seq(0, 2, 0),
                  Seq(1, 2, 0),
                  Seq(2, 2, 0)
              )
    }

    "evolve" should "give correct result for example" in {
        evolve(parse(example.linesIterator)) shouldBe Set(
            Seq(0, 1, -1),
            Seq(2, 2, -1),
            Seq(1, 3, -1),

            Seq(0, 1, 0),
            Seq(2, 1, 0),
            Seq(1, 2, 0),
            Seq(2, 2, 0),
            Seq(1, 3, 0),

            Seq(0, 1, 1),
            Seq(2, 2, 1),
            Seq(1, 3, 1)
        )
    }

    "neighbourhood" should "calculate neighboorhood" in {
        neighbourhood(Seq(0, 1)) shouldBe 
            Seq(
                Seq(-1, 0), Seq(0, 0), Seq(1, 0),
                Seq(-1, 1), Seq(0, 1), Seq(1, 1),
                Seq(-1, 2), Seq(0, 2), Seq(1, 2)
            )
    }
}
