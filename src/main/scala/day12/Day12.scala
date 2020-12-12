package day12

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import cats.implicits._
import cats.syntax._

final case class Point(
    x: Double,
    y: Double
) {
    def + (delta: Vector) = Point(x + delta.dx, y + delta.dy)
}

final case class Vector(
    dx: Double,
    dy: Double
) {
    def + (other: Vector) = Vector(dx + other.dx, dy + other.dy)

    def * (multiple: Double) = Vector(dx * multiple, dy * multiple)

    lazy val magnitude: Double = math.sqrt(dx * dx + dy * dy)
    lazy val angle: Double = math.atan2(dy, dx)

    def rotate(theta: Double) = 
        Vector(math.cos(angle + theta) * magnitude, math.sin(angle + theta) * magnitude)
    
}

final case class Boat(
    location: Point,
    heading: Vector
) {
    def move(vector: Vector) = copy(location = location + vector)
}

sealed trait Instruction
final case class N(value: Int) extends Instruction
final case class S(value: Int) extends Instruction
final case class E(value: Int) extends Instruction
final case class W(value: Int) extends Instruction
final case class L(value: Int) extends Instruction
final case class R(value: Int) extends Instruction
final case class F(value: Int) extends Instruction


object Day12 {
    def followPath(path: Seq[Instruction]): Boat =
        path.foldLeft(Boat(Point(0, 0), Vector(1, 0))) { (boat, instruction) =>
            instruction match {
                case N(v) => boat.move(Vector(0, v))
                case S(v) => boat.move(Vector(0, -v))
                case E(v) => boat.move(Vector(v, 0))
                case W(v) => boat.move(Vector(-v, 0))
                case L(v) => boat.copy(heading = boat.heading.rotate(v.toRadians))
                case R(v) => boat.copy(heading = boat.heading.rotate(-v.toRadians))
                case F(v) => boat.move(boat.heading * v)
            }
        }

    def followPathWithWaypoint(path: Seq[Instruction]): Boat =
        path.foldLeft(Boat(Point(0, 0), Vector(10, 1))) { (boat, instruction) => { 
            instruction match {
                case N(v) => boat.copy(heading = boat.heading + Vector(0, v))
                case S(v) => boat.copy(heading = boat.heading + Vector(0, -v))
                case E(v) => boat.copy(heading = boat.heading + Vector(v, 0))
                case W(v) => boat.copy(heading = boat.heading + Vector(-v, 0))
                case L(v) => boat.copy(heading = boat.heading.rotate(v.toRadians))
                case R(v) => boat.copy(heading = boat.heading.rotate(-v.toRadians))
                case F(v) => boat.move(boat.heading * v)
            }}
        }

    val pattern = raw"(N|S|E|W|L|R|F)([0-9]+)".r

    def parse(line: String): Option[Instruction] = line match {
        case pattern("N", v) => Some(N(v.toInt))
        case pattern("S", v) => Some(S(v.toInt))
        case pattern("E", v) => Some(E(v.toInt))
        case pattern("W", v) => Some(W(v.toInt))
        case pattern("L", v) => Some(L(v.toInt))
        case pattern("R", v) => Some(R(v.toInt))
        case pattern("F", v) => Some(F(v.toInt))
        case _ => None
    }
}

object Part1 extends IOApp {
    import Day12._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day12/input.txt")
        instructions = input.toSeq.traverse(parse)
        exitcode <- Util.execute(instructions.map(followPath)
            .map(boat => (math.abs(boat.location.x) + math.abs(boat.location.y)).toInt))
    } yield exitcode
}

object Part2 extends IOApp {
    import Day12._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day12/input.txt")
        instructions = input.toSeq.traverse(parse)
        exitcode <- Util.execute(instructions.map(followPathWithWaypoint)
            .map(boat => (math.abs(boat.location.x) + math.abs(boat.location.y)).round))
    } yield exitcode
}
