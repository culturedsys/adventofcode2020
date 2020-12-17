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

    def matchingRules(rules: Seq[Rule], ticket: Ticket): Seq[Set[Rule]] = 
        ticket.values.map(value => rules.filter(_.valid(value)).toSet)

    def intersectMany[A](lefts: Seq[Set[A]], rights: Seq[Set[A]]): Seq[Set[A]] =
        lefts.zip(rights).map { case (left, right) => left.intersect(right) }

    def expand[T](l: List[List[T]]): List[List[T]] = l match {
        case Nil => List(Nil)
        case head :: tail => head.flatMap(h => expand(tail).map(h :: _))
    }
        
    def uniqueMatches(s: Seq[Set[String]]):Seq[String] = {
        val singulars = s.filter(_.size == 1)
        if (singulars.length == s.length) s.map(_.head)
        else uniqueMatches(s.map { c =>
            if (singulars.contains(c)) c
            else singulars.foldLeft(c)((x, y) => x.diff(y))
        }) 
    }

    def matchFields(rules: Seq[Rule], tickets: Seq[Ticket]): Map[String, Int] = {       
        val matchSets = tickets.map(ticket => matchingRules(rules, ticket))
            .filter(!_.exists(_.isEmpty))
            .reduce((left, right) => intersectMany(left, right))
            .map(_.map(_.name))

        uniqueMatches(matchSets).zipWithIndex.toMap
    }

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

object Part2 extends IOApp {
    import Day16._ 
 
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- IO { Source.fromResource("day16/input.txt").mkString }
        parsed = notes.parseAll(input)
        exitcode <- Util.execute(parsed.map {
            case (rules, myticket, tickets) => {
                val map = matchFields(rules, tickets)
                map.filter { case (k, v) => k.startsWith("departure")}
                    .values.map(i => myticket.values(i).toLong)
                    .product
            }
        }.toOption)
    } yield ExitCode.Error


}