package day14

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

sealed trait Instruction
final case class Mask(mask: String) extends Instruction {
    private def setZero(a: Long, i: Int): Long =  a & ~(1L << i)
    private def setOne(a: Long, i: Int): Long = a | (1L << i)

    def applyFixed(value: Long): Long = 
        mask.reverse.zipWithIndex.foldLeft(value)((a, c) => c match {
            case ('0', i) => setZero(a, i)
            case ('1', i) => setOne(a, i)
            case _ => a
        })

    def applyFloating(value: Long): Set[Long] = 
        mask.reverse.zipWithIndex.foldLeft(Set(value))((a, c) => c match {
            case ('0', i) => a
            case ('1', i) => a.map(setOne(_, i))
            case (_, i) => a.flatMap(x => Set(setZero(x, i), setOne(x, i)))          
        })
}

final case class Store(address: Long, value: Long) extends Instruction

object Day14 {
    val setPattern = raw"mem\[([0-9]+)\] = ([0-9]+)".r
    val maskPattern = raw"mask = ([X01]+)".r

    def parse(line: String): Instruction = line match {
        case setPattern(address, value) => Store(address.toLong, value.toLong)
        case maskPattern(mask) => Mask(mask)
    }

    def runFixed(instructions: Seq[Instruction]): Map[Long, Long] = {
        instructions.foldLeft((Mask(""), Map[Long, Long]())) { (acc, instruction) =>
            val (mask, memory) = acc
            instruction match {
                case Store(address, value) => (mask, memory + (address -> mask.applyFixed(value)))
                case newMask @ Mask(_) => (newMask, memory)
            }
        }._2
    }

    def runFloating(instructions: Seq[Instruction]): Map[Long, Long] = {
        instructions.foldLeft((Mask(""), Map[Long, Long]())) { (acc, instruction) =>
            val (mask, memory) = acc
            instruction match {
                case Store(address, value) =>
                    (mask, mask.applyFloating(address).foldLeft(memory)((m, address) => (m + (address -> value))))
                case newMask @ Mask(_) => (newMask, memory)
            }
        }._2
    }
}

object Part1 extends IOApp {
    import Day14._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day14/input.txt")
        instructions = input.map(parse).toSeq
        exitcode <- Util.execute {
            Some(Day14.runFixed(instructions).values.sum)
        }
    } yield exitcode

}

object Part2 extends IOApp {
    import Day14._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day14/input.txt")
        instructions = input.map(parse).toSeq
        exitcode <- Util.execute {
            Some(Day14.runFloating(instructions).values.sum)
        }
    } yield exitcode
}