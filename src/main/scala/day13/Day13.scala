package day13

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import java.{util => ju}

object Day13 {
    def lowestMultipleGreaterThan(base: Int, target: Int): Int =
        ((target / base) + 1) * base

    def lowestMultipleGreaterThan(candidates: Seq[Int], target: Int): (Int, Int) = 
        candidates.map(base => (base, lowestMultipleGreaterThan(base, target))).sortBy(_._2).head

    def multiples(base: Int): Stream[Long] = 
        Stream.from(1).map(_ * base)

    def findMatching(constraintsWithOptions: Seq[(Option[Int], Int)]): Long = {
        val constraints = constraintsWithOptions.flatMap {
            case (None, _) => Seq()
            case (Some(base), offset) => Seq((base, offset)) 
        }

        def multiplesAfter(base: Long, start: Long): Iterator[Long]
            = Iterator.iterate(start + base)(_ + base)

        def go(candidates: Iterator[Long], matched: Seq[Int]): Long = {
            val candidate = candidates.next()

            val matching = constraints.filter {
                case (base, offset) => (candidate + offset) % base == 0 
            }
            
            if (matching.length == constraints.length)
                candidate
            else if (matching.filter(c => !matched.contains(c._1)).length > 0) {
                val step = matching.map[Long](_._1).reduce(_ * _)
                go(multiplesAfter(step, candidate), matching.map(_._1))
            } else
                go(candidates, matched)
        }

        go(multiplesAfter(constraints.head._1, 0), Seq(constraints.head._1))
    }

    def parse(input: Iterator[String]): (Int, Seq[Option[Int]]) = 
        (input.next().toInt, input.next().split(",").map(_.toIntOption).toSeq)
}

object Part1 extends IOApp {
    import Day13._
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day13/input.txt")
        (target, candidates) = parse(input)
        exitcode <- Util.execute {
            val (base, multiple) = lowestMultipleGreaterThan(candidates.flatten, target)
            Some(base * (multiple - target))
        }
    } yield exitcode
}

object Part2 extends IOApp {
    import Day13._ 
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day13/input.txt")
        (target, candidates) = parse(input)
        exitcode <- Util.execute {
            Some(findMatching(candidates.zipWithIndex))
        }
    } yield exitcode
}

