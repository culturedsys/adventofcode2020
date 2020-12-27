package day24

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

sealed trait Direction
case object E extends Direction
case object SE extends Direction
case object SW extends Direction
case object W extends Direction
case object NW extends Direction
case object NE extends Direction

object Direction {
    val all = Set(E, SE, SW, W, NW, NE)
}

object Day24 {
    type Point = (Int, Int)

    def relative(location: Point, direction: Direction): Point = { 
        val (x, y) = location
        direction match {
            case E => (x + 1, y)
            case SE => (x, y + 1)
            case SW => (x - 1, y + 1)
            case W => (x - 1 , y)
            case NW => (x, y - 1)
            case NE => (x + 1, y - 1)
        }
    }

    def adjacents(location: Point) = 
        Direction.all.map(d => relative(location, d))

    def followPath(path: Seq[Direction]): Point = 
        path.foldLeft((0, 0))(relative)

    def flip(blackPoints: Set[Point], target: Point): Set[Point] = 
        if (blackPoints.contains(target))
            blackPoints - target
        else
            blackPoints + target

    def evolve(blackPoints: Set[Point]) = {
        val candidates = blackPoints ++ blackPoints.flatMap(adjacents)

        candidates.filter { point =>
            val adjacentCount = adjacents(point).count(blackPoints.contains(_))
            if (blackPoints.contains(point)) {
                adjacentCount == 1 || adjacentCount == 2
            } else
                adjacentCount == 2
        }
    }

    def parse(input: String): Seq[Direction] = { 
        def go(acc: Seq[Direction], remaining: Seq[Char]): Seq[Direction] = remaining match {
            case Nil => acc
            case 'e' +: rest => go(E +: acc, rest)
            case 's' +: 'e' +: rest => go(SE +: acc, rest)
            case 's' +: 'w' +: rest => go(SW +: acc, rest)
            case 'w' +: rest => go(W +: acc, rest)
            case 'n' +: 'w'  +: rest => go(NW +: acc, rest)
            case 'n' +: 'e' +: rest => go(NE +: acc, rest)
        }

        go(Seq(), input.toSeq).reverse
    }
}

object Part1 extends IOApp {
    import Day24._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day24/input.txt")
        paths = input.map(parse)
        exitcode <- Util.execute {
            val blackPoints = paths.foldLeft(Set[Point]()) { (blackPoints, path) => 
                flip(blackPoints, followPath(path))
            }

            Some(blackPoints.size)
        }
    } yield exitcode    
}

object Part2 extends IOApp {
    import Day24._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day24/input.txt")
        paths = input.map(parse)
        initial = paths.foldLeft(Set[Point]()) { (blackPoints, path) => 
            flip(blackPoints, followPath(path))
        }
        exitcode <- Util.execute {
            val result = Stream.iterate(initial)(evolve).drop(100).head
            Some(result.size)
        }
    } yield exitcode    
}
