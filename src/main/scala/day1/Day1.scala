package day1

import util.Util.report

import cats.effect._
import scala.io.Source
import cats._
import cats.implicits._
import cats.syntax._
import scala.annotation.tailrec

object Day1 {
    def findPairSummingTo(candidates: Set[Int], target: Int): Option[(Int, Int)] = {
        candidates.find(candidate => candidates.contains(target - candidate))
            .map(result => (result, target - result))
    }

    def findTripleSummingTo(candidates: Set[Int], target: Int): Option[(Int, Int, Int)] = {
        def go(remaining: Set[Int]): Option[(Int, Int, Int)] = 
            remaining.headOption
                .flatMap(candidate => findPairSummingTo(candidates, target - candidate) match {
                    case None => go(remaining.tail)
                    case Some((x, y)) => Some(candidate, x, y) 
                })

       go(candidates)
    }

    def readInput: IO[Seq[Int]] = IO {
        Source.fromResource("day1/input.txt")
            .getLines()
            .map(_.toInt)
            .toSeq
    }
}

object Part1 extends IOApp {
    import Day1._

    def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = findPairSummingTo(input.toSet, 2020)
                .map { case (x, y)  => x * y}
        exitcode <- report(result)
    } yield exitcode
}

object Part2 extends IOApp {
    import Day1._

    def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = findTripleSummingTo(input.toSet, 2020)
                .map { case (x, y, z) => x * y * z }
        exitcode <- report(result)
    } yield exitcode
}