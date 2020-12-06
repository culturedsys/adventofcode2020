package day6

import util.Util
import cats.effect.IO
import scala.io.Source
import cats.effect.IOApp
import cats.effect.ExitCode

object Day6 {
    def parse(lines: Iterator[String]): Seq[Seq[String]] =
        Util.splitIf[String](_.trim().isEmpty, lines)
        
    def countCharsInAnyLine(lines: Seq[String]): Int =
        lines.map(_.toSet)
                .reduce(_.union(_))
                .size

    def countCharsInAllLines(lines: Seq[String]): Int =
        lines.map(_.toSet)
                .reduce(_.intersect(_))
                .size

    def readInput: IO[Iterator[String]] = IO {
        Source.fromResource("day6/input.txt")
            .getLines()
    }
}

object Part1 extends IOApp {
    import Day6._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        groups = parse(input)
        result = groups.map(countCharsInAnyLine).sum
        exitcode <- Util.report(Some(result))
    } yield exitcode
}

object Part2 extends IOApp {
    import Day6._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        groups = parse(input)
        result = groups.map(countCharsInAllLines).sum
        exitcode <- Util.report(Some(result))
    } yield exitcode
}
