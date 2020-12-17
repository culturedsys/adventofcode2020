package day16

import cats.parse.{Parser => P}
import cats.data.NonEmptyList
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util
import scala.annotation.tailrec

final case class Rule (
    name: String,
    ranges: Seq[Range]
) {
    def valid(n: Int): Boolean = 
        ranges.exists(_.contains(n))
}

final case class Ticket(values: Seq[Int])

object Parser {
    val number = P.charsWhile1(_.isDigit).map(_.toInt)
    val whitespace = P.charWhere(_.isSpaceChar).rep1.void
    val eol = P.char('\n').void

    val range = (number ~ (P.char('-') *> number)).map {
        case (low, high) => low to high
    }
    val ranges = P.rep1Sep(range, 1, whitespace ~ P.string("or") ~ whitespace)
    val rule = (P.charsWhile1(_ != ':') ~ ((P.char(':') ~ whitespace) *> ranges)).map {
        case (name, ranges) => Rule(name.toString(), ranges.toList)
    }
    val rules = P.rep1Sep(rule.backtrack, 1, eol)

    val ticket = P.rep1Sep(number, 1, P.char(',')).map(n => Ticket(n.toList))
    val tickets = P.rep1Sep(ticket, 1, eol)

    val yourTicketSep = P.string1("your ticket:").void
    val yourTicket = (yourTicketSep ~ eol) *> ticket
    
    val nearbyTicketSep = P.string1("nearby tickets:").void
    val nearbyTickets = (nearbyTicketSep ~ eol) *> tickets
    val notes = (
        (rules <* (eol ~ eol)) ~ 
        (yourTicket <* (eol ~ eol)) ~
        (nearbyTickets <* eol.?)).map {
            case ((rules, your), nearby) => (rules.toList, your, nearby.toList)
        }
}

object Day16 {
    def invalidValues(rules: Seq[Rule], tickets: Seq[Ticket]) =
        tickets.flatMap(_.values.filter(value => !rules.exists(_.valid(value))))

    def matchingRules(rules: Seq[Rule], ticket: Ticket): Seq[Set[Rule]] = 
        ticket.values.map(value => rules.filter(_.valid(value)).toSet)

    def intersectMany[A](lefts: Seq[Set[A]], rights: Seq[Set[A]]): Seq[Set[A]] =
        lefts.zip(rights).map { case (left, right) => left.intersect(right) }
        
    @tailrec
    def uniqueMatches(sets: Seq[Set[Rule]]):Seq[Rule] = {
        val uniques = sets.filter(_.size == 1).map(_.head)
        if (uniques.length == sets.length) 
            uniques
        else {
            val removingFoundUniques = sets.map { set =>
                if (set.size == 1) set
                else uniques.foldLeft(set)((acc, singular) => acc - singular)
            }
            uniqueMatches(removingFoundUniques)
        } 
    }

    def matchFields(rules: Seq[Rule], tickets: Seq[Ticket]): Map[String, Int] = {       
        val matchSets = tickets.map(ticket => matchingRules(rules, ticket))
            .filter(_.forall(!_.isEmpty))
            .reduce((left, right) => intersectMany(left, right))
            
        uniqueMatches(matchSets)
            .map(_.name)
            .zipWithIndex
            .toMap
    }

    def readInput = IO { Source.fromResource("day16/input.txt").mkString }
}

object Part1 extends IOApp {
    import Day16._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        parsed = Parser.notes.parseAll(input)
        exitcode <- Util.executeEither(parsed.map {
            case (rules, _, tickets) => invalidValues(rules, tickets).sum
        })
    } yield ExitCode.Error


}

object Part2 extends IOApp {
    import Day16._ 
 
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- readInput
        parsed = Parser.notes.parseAll(input)
        exitcode <- Util.executeEither(parsed.map {
            case (rules, myticket, tickets) => {
                val map = matchFields(rules, tickets)
                map.filter { case (k, v) => k.startsWith("departure")}
                    .values.map(i => myticket.values(i).toLong)
                    .product
            }
        })
    } yield ExitCode.Error


}