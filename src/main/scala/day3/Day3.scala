package day3

import util.Util
import cats.syntax._
import cats.implicits._
import cats._
import scala.io.Source
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

sealed trait Tile 

object Tile {
    final case object Empty extends Tile
    final case object Tree extends Tile
}

case class Field(tiles: Vector[Vector[Tile]]) {
    def apply(row: Int, col: Int): Option[Tile] = {
        if (row >= tiles.length)
            None
        else {
            val rowData = tiles(row)
            Some(rowData(col % rowData.length))    
        }             
    }
}

object Day3 {
    def parseLine(line: String): Option[Vector[Tile]] ={
        val mapper: PartialFunction[Char, Tile] =  {
            case '.' => Tile.Empty
            case '#' => Tile.Tree 
        }

        line.iterator.toSeq.map(mapper.lift)
            .sequence
            .map(_.toVector)
    }

    def parse(lines: Seq[String]): Option[Vector[Vector[Tile]]]  = 
        lines.map(line => parseLine(line.stripLineEnd))
            .sequence
            .map(_.toVector) 

    def countCollisions(field: Field, startRow: Int, startCol: Int, vertSlope: Int, horizSlope: Int): Int = {
        def go(row: Int, col: Int, running: Int): Int = field(row, col) match {
            case None => running
            case Some(Tile.Empty) => go(row + vertSlope, col + horizSlope, running)
            case Some(Tile.Tree) => go(row + vertSlope, col + horizSlope, running + 1) 
        }

        go(0, 0, 0)
    }

    def countForAllSlopes(field: Field, slopes: Seq[(Int, Int)]): Long = {
        slopes.map {
            case (down, right) => countCollisions(field, 0, 0, down, right)
        }.map(_.toLong).product
    }

    def readInput: IO[Seq[String]] = IO {
        Source.fromResource("day3/input.txt").getLines().toSeq
    }
}

object Part1 extends IOApp {
    import Day3._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = parse(input)
            .map(field => countCollisions(Field(field), 0, 0, 1, 3))
        exitcode <- Util.report(result)
    } yield exitcode
}

object Part2 extends IOApp {
    import Day3._

    val slopes = Seq(
        (1, 1),
        (1, 3),
        (1, 5),
        (1, 7),
        (2, 1)
    )

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        result = parse(input)
            .map(field => countForAllSlopes(Field(field), slopes))
        exitcode <- Util.report(result)
    } yield exitcode
}