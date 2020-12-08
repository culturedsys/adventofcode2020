package day8

import scala.io.Source
import cats.effect.IO
import cats.implicits._
import cats.syntax._
import cats.effect.IOApp
import cats.effect.ExitCode
import util.Util

sealed trait Instruction
object Instruction {
    final case class Nop(arg: Int) extends Instruction
    final case class Acc(arg: Int) extends Instruction
    final case class Jmp(arg: Int) extends Instruction
}

final case class Registers(pc: Int, acc: Int)

object Day8 {
    import Instruction._

    def run(instructions: Vector[Instruction]): Stream[Registers] =
        Stream.iterate(Registers(0, 0))(registers => instructions(registers.pc) match {
            case Nop(_) => registers.copy(pc = registers.pc + 1)
            case Acc(arg) => registers.copy(pc = registers.pc + 1, acc = registers.acc + arg)
            case Jmp(arg) => registers.copy(pc = registers.pc + arg)
        }) 

    def runToRepetition(instructions: Vector[Instruction]): Stream[Registers] = {
        val states = run(instructions)
        val visited = states.scanLeft(Set[Int]())((s, r) => s + r.pc)
        states.drop(1)
            .zip(visited)
            .takeWhile {
                    case (Registers(pc, _), visited) => !visited.contains(pc)
            }.map(_._1)
    }

    def runToExit(instructions: Vector[Instruction]): Option[Registers] = 
        runToRepetition(instructions).find(_.pc == instructions.length)

    def findTerminatingPermutation(instructions: Vector[Instruction]): Option[Registers] = {
        val permutations = (0 until instructions.length).map(i => instructions(i) match {
            case Jmp(arg) => instructions.updated(i, Nop(arg))
            case Nop(arg) => instructions.updated(i, Jmp(arg))
            case _ => instructions
        })

        permutations.map(runToExit)
            .find(_.isDefined)
            .flatten
    }

    def parseInstruction(instruction: String): Option[Instruction] = instruction.split(" ").toSeq match {
        case Seq("nop", arg) => arg.toIntOption.map(Nop)
        case Seq("acc", arg) => arg.toIntOption.map(Acc)
        case Seq("jmp", arg) => arg.toIntOption.map(Jmp)
        case _ => None
    }

    def parse(lines: Iterator[String]): Option[Vector[Instruction]] =
        lines.map(parseInstruction).toVector.sequence

    def readInput = IO {
        Source.fromResource("day8/input.txt").getLines()
    }
}

object Part1 extends IOApp {
    import Day8._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        instructions = parse(input)
        result = instructions.map(runToRepetition(_).last.acc)
        exitcode <- Util.report(result)
    } yield exitcode
}

object Part2 extends IOApp {
    import Day8._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        instructions = parse(input)
        result = instructions.flatMap(findTerminatingPermutation(_).map(_.acc))
        exitcode <- Util.report(result)
    } yield exitcode
}
