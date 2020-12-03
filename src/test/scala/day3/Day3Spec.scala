package day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day3Spec extends AnyFlatSpec with should.Matchers {
    import Day3._

    "parseLine" should "give the correct answer for example" in {
        import Tile._
        parseLine("..#.#") shouldBe Some(Vector(Empty, Empty, Tree, Empty, Tree))
    }

    "parse" should "give correct answer for example" in {
        import Tile._
        parse(
            """.#
              |#.""".stripMargin.linesIterator.toSeq) shouldBe Some(Vector(Vector(Empty, Tree), Vector(Tree, Empty)))
    }

    val example = 
        """..##.......
            |#...#...#..
            |.#....#..#.
            |..#.#...#.#
            |.#...##..#.
            |..#.##.....
            |.#.#.#....#
            |.#........#
            |#.##...#...
            |#...##....#
            |.#..#...#.#""".stripMargin.linesIterator.toSeq

    "countCollisions" should "give correct answer for example" in {
        parse(example)
            .map(Field.apply)
            .map(field => countCollisions(field, 0, 0, 1, 3)) shouldBe Some(7)
    }

    "countCollisions" should "give correct answer for extended examples" in {
        val result = parse(example)
            .map(Field.apply)
            .map(field => Seq(
                (1, 1),
                (1, 3),
                (1, 5),
                (1, 7),
                (2, 1)
                ).map {
                    case (down, right) => countCollisions(field, 0, 0, down, right) 
                })   

        result shouldBe Some(Seq(2, 7, 3, 4, 2))
    }
}
