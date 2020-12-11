package day11

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

sealed trait Square
case object Occupied extends Square
case object Empty extends Square
case object Floor extends Square

case class Field(width: Int, height: Int, squares: Vector[Square]) {
    def apply(x: Int, y: Int): Option[Square] = 
        if (x < 0 || x >= width || y < 0 || y >= height)
            None
        else
            Some(squares(x + y * width))

    def countAdjacent(x: Int, y: Int): Int =
        (y - 1 to y + 1).flatMap(yy => 
            (x - 1 to x + 1).map( xx => 
                if (x == xx && y == yy) 
                    0 
                else if (this(xx, yy).getOrElse(Floor) != Occupied)
                    0
                else
                    1
            )
        ).sum

    def countOccupied = squares.count(_ == Occupied)

    def evolveState: Field = {
        val newSquares = (0 until height).flatMap { y =>
            (0 until width).map { x =>
                val current = this(x, y).get
                current match {
                    case Floor => Floor
                    case Occupied => if (countAdjacent(x, y) >= 4) Empty else Occupied
                    case Empty => if (countAdjacent(x, y) == 0) Occupied else Empty
                }
            }
        }.toVector
        Field(width, height, newSquares)
    }
}

object Day11 {
    def parse(lines: Iterator[String]): Field = {
        val (width, height, squares) = lines.foldLeft(0, 0, Vector[Square]())((acc, line) => {
            val (width, height, squares) = acc
            val newSquares = squares ++ line.map[Square]({
                case '#' => Occupied
                case 'L' => Empty
                case '.' => Floor 
            }).toVector
            val newWidth = math.max(width, line.length)
            (newWidth, height + 1, newSquares)
        })

        Field(width, height, squares)
    }

    def findSteadyState(field: Field): Field = {
        val evolution = Stream.iterate(field)(_.evolveState)
        evolution.zip(evolution.tail)
            .dropWhile { case (first, second) => first != second}
            .head._1
    }
}

object Part1 extends IOApp {
    import Day11._
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day11/input.txt")
        field = parse(input)
        steadyState = findSteadyState(field)
        exitcode <- Util.execute(Some(steadyState.countOccupied))
    } yield exitcode
}