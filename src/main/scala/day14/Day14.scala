package day14

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

sealed trait Instruction
final case class Mask(mask: String) extends Instruction {
    private def setZero(a: Long, i: Int): Long =  a & ~(1L << i)
    private def setOne(a: Long, i: Int): Long = a | (1L << i)

    def apply[T](initial: T): ((T, (Char, Int)) => T) => T =
        mask.reverse.zipWithIndex.foldLeft(initial) _

    def applyFixed(value: Long): Long = 
        apply(value)((a, c) => c match {
            case ('0', i) => setZero(a, i)
            case ('1', i) => setOne(a, i)
            case _ => a
        })

    def applyFloating(value: Long): Set[Long] = 
        apply(Set(value))((a, c) => c match {
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

    type Memory = Map[Long, Long]
    object Memory {
        def apply(): Memory = Map()
    }

    def storeWithFixedMask(memory: Memory, address: Long, value: Long, mask: Mask): Memory =
        memory + (address -> mask.applyFixed(value))

    def storeWithFloatingMask(memory: Memory, address: Long, value: Long, mask: Mask) =
        mask.applyFloating(address).foldLeft(memory)((m, address) => (m + (address -> value)))

    def run(instructions: Seq[Instruction],
            store: (Memory, Long, Long, Mask) => Memory): Memory = {
        instructions.foldLeft((Mask(""), Memory())) { (acc, instruction) =>
            val (mask, memory) = acc
            instruction match {
                case Store(address, value) => (mask, store(memory, address, value, mask))
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
            Some(Day14.run(instructions, storeWithFixedMask).values.sum)
        }
    } yield exitcode

}

object Part2 extends IOApp {
    import Day14._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day14/input.txt")
        instructions = input.map(parse).toSeq
        exitcode <- Util.execute {
            Some(Day14.run(instructions, storeWithFloatingMask).values.sum)
        }
    } yield exitcode
}