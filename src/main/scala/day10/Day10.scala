package day10

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits._
import cats.syntax._
import util.Util
import cats.data.State
import cats.effect.Clock
import java.util.concurrent.TimeUnit
import cats.Show


object Day10 {
    def countGaps(seq: Seq[Int]): Map[Int, Int] = {
        val sorted = seq.sorted.view
        (0 +: sorted).zip(sorted)
            .groupMapReduce { case (first, second) => second - first } (_ => 1)(_ + _)
    }

    def productOfGaps(seq: Seq[Int]): Option[Int] = {
        val gaps = countGaps(seq)
        for {
            oneCount <- gaps.get(1)
            threeCount <- gaps.get(3)
        } yield oneCount * (threeCount + 1)
    }

    def pure(value: Long): State[Map[Int, Long], Long] = State.pure(value)

    def countPaths(seq: Seq[Int]): Long = {
        val sorted = 0 +: seq.sorted.toVector
        
        def countPathsFrom(i: Int): State[Map[Int, Long], Long] = for {
            cache <- State.get
            result <- cache.get(i) match {
                case Some(value) => pure(value)
                case None => {
                    val slice = sorted.drop(i)
                    val candidates = slice.tail.takeWhile(_ <= slice.head + 3)
                    if (candidates.isEmpty)
                        pure(1L)
                    else
                        (1 to candidates.length).toList
                            .traverse(x => countPathsFrom(i + x))
                            .map(_.sum)
                }
            }
            _ <- State.modify((cache: Map[Int, Long]) => cache + (i -> result))
        } yield result

        countPathsFrom(0).runA(Map()).value
    }
}

object Part1 extends IOApp {
    import Day10._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day10/input.txt")
        numbers = input.toSeq.traverse(_.toIntOption)
        exitcode <- Util.execute { numbers.flatMap(productOfGaps(_)) }
    } yield exitcode
}
 
object Part2 extends IOApp {
    import Day10._
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day10/input.txt")
        numbers = input.toSeq.traverse(_.toIntOption)
        exitcode <- Util.execute { numbers.map(countPaths(_)) }
    } yield exitcode
}