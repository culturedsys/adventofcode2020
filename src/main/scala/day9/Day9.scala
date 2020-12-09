package day9

import cats.data.Chain
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import cats.implicits._
import cats.syntax._
import util.Util
import cats.Functor
import cats.Traverse
import scala.annotation.tailrec

object Day9 {
    def isNumberValid(preamble: Seq[Long], candidate: Long): Boolean =
        preamble.combinations(2)
            .map(_.sum) 
            .contains(candidate)

    def findFirstInvalid(data: Seq[Long], preambleLength: Int): Option[Long] = 
        data.sliding(preambleLength + 1, 1)
            .map(_.splitAt(preambleLength))
            .find { case (preamble, Seq(candidate)) => !isNumberValid(preamble, candidate) }
            .map { case (_, Seq(found)) => found }

    def findSequenceSummingTo(data: Seq[Long], target: Long): Option[Vector[Long]] = {
        @tailrec
        def go(sequence: Vector[Long], remaining: Seq[Long]): Option[Vector[Long]] ={
            val sum = sequence.foldLeft(0L)(_ + _)
            if (sum == target)
                Some(sequence)
            else if (sum > target)
                go(sequence.tail, remaining)
            else 
                remaining match {
                    case head +: tail => go(sequence :+ head, tail)
                    case Seq() => None
                }
        }

        go(Vector(), data)
    }

    def readInput = IO {
        Source.fromResource("day9/input.txt")
            .getLines()
    }
}

object Part1 extends IOApp { 
    import Day9._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = input.toSeq.traverse(_.toLongOption)
                    .flatMap(findFirstInvalid(_, 25))
        exitcode <- Util.report(result)
    } yield exitcode
}

object Part2 extends IOApp { 
    import Day9._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = input.toSeq.traverse(_.toLongOption)
                    .flatMap(findSequenceSummingTo(_, 41682220L))
                    .map(sequence => sequence.min + sequence.max)
        exitcode <- Util.report(result)
    } yield exitcode
}