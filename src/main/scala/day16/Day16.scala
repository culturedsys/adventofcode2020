package day16

import cats.parse.{Parser => P}
import cats.data.NonEmptyList
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util

final case class Rule (
    name: String,
    ranges: Seq[Range]
) {
    def valid(n: Int): Boolean = 
        ranges.exists(_.contains(n))
}

final case class Ticket(values: Seq[Int])

object Day16 {
    def invalidValues(rules: Seq[Rule], tickets: Seq[Ticket]) =
        tickets.flatMap(_.values.filter(value => !rules.exists(_.valid(value))))

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

object Part1 extends IOApp {
    import Day16._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- IO { Source.fromResource("day16/input.txt").mkString }
        parsed = notes.parseAll(input)
        _ <- IO { parsed match {
            case Left(err) => println(err)
            case r => r
        }}
        exitcode <- Util.execute(parsed.map {
            case (rules, _, tickets) => invalidValues(rules, tickets).sum
        }.toOption)
    } yield ExitCode.Error


}