package day1

import cats.effect._
import scala.io.Source
import scala.util.control.TailCalls
import cats._
import cats.implicits._
import cats.syntax._

object Day1 extends IOApp {
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

    def run(args: List[String]): IO[ExitCode] = {
        for {
            input <- readInput
            result = Seq(
                findPairSummingTo(input.toSet, 2020)
                    .map { case (x, y)  => x * y}
                    .toRight("Error in part 1"),
                findTripleSummingTo(input.toSet, 2020)
                    .map { case (x, y, z) => x * y * z }
                    .toRight("Error in part 2")
            ).sequence
            exitcode <- result match {
                case Left(msg) => IO {
                    println(msg)
                    ExitCode.Error
                }
                case Right(Seq(part1, part2)) => IO { 
                    println(part1)
                    println(part2)
                    ExitCode.Success 
                }
            }
        } yield exitcode
    }
}
