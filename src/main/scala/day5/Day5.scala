package day5

import cats.syntax._
import cats.implicits._
import cats.Applicative
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import scala.io.Source

sealed trait Half
final case object Lower extends Half
final case object Upper extends Half

object Day5 {
    def midpoint(range: Range): Int = range.start + range.length / 2

    def binaryPartition(path: Seq[Half], range: Range): Int = path match {
        case Nil => range.start
        case Lower :: tail => binaryPartition(tail, range.start until midpoint(range))
        case Upper :: tail => binaryPartition(tail, midpoint(range) until range.end)
    }

    def parseHalf(lower: Char, upper: Char) = 
        (c: Char) => if (c == lower) Some(Lower) else if (c == upper) Some(Upper) else None

    def parse(input: String): Option[(Seq[Half], Seq[Half])] = input.splitAt(7) match {
        case (row, seat) => 
            (
                row.toList.traverse(parseHalf('F', 'B')), 
                seat.toList.traverse(parseHalf('L', 'R'))
            ).mapN(new Tuple2(_, _))
    }

    def calculateSeatIds(seatNumbers: Iterator[String]) = seatNumbers.map(parse).map( _.map { 
        case (rowSeq, colSeq) => binaryPartition(rowSeq, 0 until 128) * 8 +  binaryPartition(colSeq, 0 until 8)
    })

    def largestSeatId(seatNumbers: Iterator[String]) = 
        calculateSeatIds(seatNumbers).max

    def missingSeatId(seatNumbers: Iterator[String]) = {
        def go(previous: Int, ns: Seq[Int]): Option[Int] = ns match {
            case Seq() => None
            case head +: tail => if (head - previous > 1) Some(previous + 1) else go(head, tail)
        }

        for {
            ids <- calculateSeatIds(seatNumbers).toSeq.sequence.map(_.sorted)
            head <- ids.headOption
            result <- go(head, ids.tail)
        } yield result
    } 

    def readInput = IO {
        Source.fromResource("day5/input.txt").getLines()
    }
}

object Part1 extends IOApp {
    import Day5._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = largestSeatId(input)
        exitcode <- Util.report(result)
    } yield exitcode
}

object Part2 extends IOApp {
    import Day5._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = missingSeatId(input)
        exitcode <- Util.report(result)
    } yield exitcode
}