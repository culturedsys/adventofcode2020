package day17

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day17 {
    def parse(lines: Iterator[String], dimensions: Int = 3): Set[Seq[Int]] = 
        lines.zipWithIndex.flatMap { case (line, y) => 
            line.zipWithIndex.map {
                case ('#', x) => Some(Seq(x, y) ++ Seq.fill(dimensions - 2)(0))
                case _ => None
            }
        }.flatten.toSet

    def neighbourhood(cell: Seq[Int]): Seq[Seq[Int]] = cell match {
        case Nil => Seq(Seq())
        case head :: tail => 
            neighbourhood(tail).flatMap(
                t => Seq(head - 1, head, head + 1).map(h => h +: t)
            )            
    }

    def evolve(state: Set[Seq[Int]]): Set[Seq[Int]] = {
        def isActive(cell: Seq[Int]) = {
            val activeInNeighbourhood = neighbourhood(cell).count(state.contains(_)) 
            if (state.contains(cell))
                activeInNeighbourhood == 3 || activeInNeighbourhood == 4
            else
                activeInNeighbourhood == 3
        }

        state.flatMap(neighbourhood).filter(isActive)
    }

    def activeAfter(state: Set[Seq[Int]], iterations: Int) = 
        Iterator.iterate(state)(evolve)
                .drop(iterations).buffered
                .head.size
}

object Part1 extends IOApp {
    import Day17._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day17/input.txt")
        state = parse(input)
        exitcode <- Util.execute {
             Some(activeAfter(state, 6))
        }
    } yield exitcode
}

object Part2 extends IOApp {
    import Day17._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day17/input.txt")
        state = parse(input, 4)
        exitcode <- Util.execute {
             Some(activeAfter(state, 6))
        }
    } yield exitcode
}