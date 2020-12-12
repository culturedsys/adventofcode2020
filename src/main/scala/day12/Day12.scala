package day12

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import cats.implicits._
import cats.syntax._

final case class Boat(
    x: Double,
    y: Double,
    heading: Double,
    waypointx: Double = 0,
    waypointy: Double = 0
) {
    def rotateWaypoint(theta: Int): Boat = 
        (if (theta < 0 ) 360 + theta else theta) match {
            case 90 => copy(waypointx = -waypointy, waypointy = waypointx)
            case 180 => copy(waypointx = -waypointx, waypointy = -waypointy)
            case 270 => copy(waypointx = waypointy, waypointy = -waypointx)
        }
    
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
        path.foldLeft(Boat(0, 0, 0)) { (boat, instruction) =>
            instruction match {
                case N(v) => boat.copy(y = boat.y + v)
                case S(v) => boat.copy(y = boat.y - v)
                case E(v) => boat.copy(x = boat.x + v)
                case W(v) => boat.copy(x = boat.x - v)
                case L(v) => boat.copy(heading = boat.heading + v)
                case R(v) => boat.copy(heading = boat.heading - v)
                case F(v) => 
                    boat.copy(x = boat.x + v * math.cos(boat.heading.toRadians), 
                        y = boat.y + v * math.sin(boat.heading.toRadians))
            }
        }

    def followPathWithWaypoint(path: Seq[Instruction]): Boat =
        path.foldLeft(Boat(0, 0, 0, 10, 1)) { (boat, instruction) => { 
            instruction match {
                case N(v) => boat.copy(waypointy = boat.waypointy + v)
                case S(v) => boat.copy(waypointy = boat.waypointy - v)
                case E(v) => boat.copy(waypointx = boat.waypointx + v)
                case W(v) => boat.copy(waypointx = boat.waypointx - v)
                case L(v) => boat.rotateWaypoint(v)
                case R(v) => boat.rotateWaypoint(-v)
                case F(v) => 
                    boat.copy(x = boat.x + boat.waypointx * v, 
                        y = boat.y + boat.waypointy * v)
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
            .map(boat => (math.abs(boat.x) + math.abs(boat.y)).toInt))
    } yield exitcode
}

object Part2 extends IOApp {
    import Day12._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day12/input.txt")
        instructions = input.toSeq.traverse(parse)
        exitcode <- Util.execute(instructions.map(followPathWithWaypoint)
            .map(boat => (math.abs(boat.x) + math.abs(boat.y)).toInt))
    } yield exitcode
}
