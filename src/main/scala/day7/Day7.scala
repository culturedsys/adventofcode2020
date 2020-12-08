package day7

import cats.parse.{Parser => P, Parser1, Numbers}
import cats.implicits._
import cats.syntax._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util
import cats.data.State
import cats.instances.tailRec
import scala.annotation.tailrec
import cats.effect.Clock
import java.util.concurrent.TimeUnit

case class Bag(
    modifier: String,
    colour: String
)

object Day7 {
    def countBagsContaining(target: Bag, rules: Map[Bag, Seq[(Int, Bag)]]): Int = {
        val rulesWithoutQuantities = rules.map {
            case (container, contents) => container -> contents.map(_._2)
        }

        def directlyContaining(bag: Bag): Set[Bag] = 
            rulesWithoutQuantities.view.flatMap {
                case (container, contents) => if (contents.contains(bag)) Set(container) else Set()
            }.toSet

        def go(results: Set[Bag], candidates: Set[Bag]): Set[Bag] = {
            val directParents = candidates.flatMap(candidate => directlyContaining(candidate))
            if (directParents.isEmpty)
                results
            else
                go(results.union(directParents), directParents.diff(results))
        }

        go(Set(), Set(target)).size
    }

    def countBagsContainedIn(container: Bag, rules: Map[Bag, Seq[(Int, Bag)]]): Int = {
        def go(container: Bag): Int = {
            val contents = rules.get(container).get
            contents.map { case (count, bag) => (go(bag) + 1) * count }.sum
        }

        go(container)
    }

    val word = P.charsWhile1(_.isLetter)
    val whitespace = P.charsWhile1(_.isWhitespace).void
    
    val bagKeyword = P.string("bag") ~ (P.char('s').?)
    val bagParser = ((word <* whitespace) ~ word <* whitespace <* bagKeyword).map {
        case (modifier, colour) => Bag(modifier, colour)
    }

    val number = P.charsWhile1(_.isDigit).map(_.toInt)

    val bagCountParser = ((number <* whitespace) ~ bagParser)

    val contentsParser = P.oneOf(List(
        P.string("no other bags").map(_ => List[(Int, Bag)]()),
        P.rep1Sep(bagCountParser, 1, P.char(',') ~ whitespace).map(_.toList)
    )).map(_.toSeq)

    val ruleParser = (bagParser <* (whitespace <* P.string("contain").void <* whitespace)) ~ contentsParser <* P.char('.')

    def parse(rule: String) = 
        ruleParser.parse(rule).map(_._2)
    
    def readInput = IO {
        Source.fromResource("day7/input.txt").getLines()
    }
}

object Part1 extends IOApp {
    import Day7._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        rules = input.toSeq.traverse(parse).map(_.toMap)
        result = rules.map(countBagsContaining(Bag("shiny", "gold"), _))
        exitcode <- Util.report(result)
    } yield exitcode
}

object Part2 extends IOApp {
    import Day7._
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput    
        rules = input.toSeq.traverse(parse).map(_.toMap)
        result = rules.map(countBagsContainedIn(Bag("shiny", "gold"), _))
        exitcode <- Util.report(result)
    } yield exitcode
}