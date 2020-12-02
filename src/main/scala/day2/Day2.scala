package day2

import util.Util.report
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode
import cats._
import cats.syntax._
import cats.implicits._
import scala.io.Source

case class Rule(start: Int, end: Int, char: Char)

object Day2 {
    def isInRange(rule: Rule, candidate: String) = 
        (rule.start to rule.end).contains(candidate.count(_ == rule.char))
  
    def isAtIndex(rule: Rule, candidate: String) = 
        candidate(rule.start - 1) == rule.char ^ candidate(rule.end - 1) == rule.char

    val syntax = raw"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r

    def parse(input: String): Option[(Rule, String)] = {
        input match {
            case syntax(min, max, char, input) => Some((Rule(min.toInt, max.toInt, char(0)), input))
            case _ => None
        }
    }

    def countValid(input: Seq[String], validator: (Rule, String) => Boolean): Option[Int] = 
        input.traverse(parse _).map { lines => 
            lines.count { case (rule, candidate) => validator(rule, candidate)}
        }

    def readInput() = IO {
        Source.fromResource("day2/input.txt")
            .getLines()
    }
}

object Part1 extends IOApp {
    import Day2._

    def run(args: List[String]): IO[ExitCode] = {
        for {
            input <- readInput
            result = countValid(input.toSeq, isInRange)
            exitcode <- report(result)
        } yield ExitCode.Error
    }
}

object Part2 extends IOApp {
    import Day2._

    def run(args: List[String]): IO[ExitCode] = {
        for {
            input <- readInput
            result = countValid(input.toSeq, isAtIndex)
            exitcode <- report(result)
        } yield ExitCode.Error
    }
}
