package day19

import cats.parse.{Parser => P}
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import cats.implicits._
import cats.syntax._
import day12.S

sealed trait Result {
    def isMatch: Boolean = false
    def isFullMatch: Boolean = false
    def isMiss: Boolean = false
}
final case class Match(remaining: Seq[Seq[Char]]) extends Result {
    override def isMatch: Boolean = true
    override def isFullMatch: Boolean = remaining.exists(_.isEmpty)
}
final case class Miss(expected: Seq[Char]) extends Result {
    override def isMiss: Boolean = true
}

sealed trait Expression {
    def recognize(input: Seq[Char])(implicit rulesMap: Map[Int, Expression]): Result
}

final case class Reference(id: Int) extends Expression {
    def recognize(input: Seq[Char])(implicit rulesMap: Map[Int, Expression]): Result = 
        rulesMap(id).recognize(input)
}

final case class Literal(char: Char) extends Expression {
    override def recognize(input: Seq[Char])(implicit rulesMap: Map[Int, Expression]): Result = input match {
        case Seq() => Miss(Seq(char))
        case head +: tail => 
            if (head == char) 
                Match(Seq(tail))
            else
                Miss(Seq(char))
    }
}

final case class Sequence(expressions: Seq[Expression]) extends Expression {
    override def recognize(input: Seq[Char])(implicit rulesMap: Map[Int, Expression]): Result = {
        def go(es: Seq[Expression], i: Seq[Char]): Result = es match {
            case Seq() => Match(Seq(i))
            case head +: rest =>
                head.recognize(i) match {
                    case m @ Miss(_) => m
                    case Match(remaining) => Match(remaining.map(go(rest, _)).flatMap {
                        case Match(remainings) => remainings
                        case _ => Seq()
                    })
                }
        }
        
        go(expressions, input)
    }
}

final case class Alternatives(alternatives: Seq[Expression]) extends Expression {
    override def recognize(input: Seq[Char])(implicit rulesMap: Map[Int, Expression]): Result = {
        def go(es: Seq[Expression], i: Seq[Char], expecteds: Seq[Char]): Result = {
            val results = es.map(_.recognize(i))
            val matches = results.flatMap { 
                case Match(remaining) => remaining  
                case _ => Seq()
            }
            if (matches.isEmpty) 
                Miss(results.flatMap { 
                    case Miss(expected) => expected ; case _ => Seq() 
                })
            else Match(matches)
        }
        go(alternatives, input, Seq())
    }
}

object Day19 {
    val whitespace = P.charWhere(_.isWhitespace).rep.void
    val literal = ((P.char('"') *> P.anyChar) <* P.char('"')).map(Literal)
    val reference = P.charsWhile1(_.isDigit).map(id => Reference(id.toInt))
    val terminal = literal.backtrack.widen[Expression] <+> reference.backtrack
    val sequence = P.rep1Sep(terminal, 1, whitespace).map(ts => Sequence(ts.toList))
    val expression = P.rep1Sep(sequence, 1, whitespace ~ P.char('|') ~ whitespace).map(ss => Alternatives(ss.toList))
    def parse(lines: Seq[String]): Either[P.Error, Map[Int, Expression]] = {
        lines.traverse { line => 
            val Array(n, rule) = line.split(":")
            expression.parseAll(rule.strip()).map((n.toInt, _))
        }.map(_.toMap)
    }
}

object Part1 extends IOApp {
    import Day19._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day19/input.txt")
        Seq(rulesLines, messages) = Util.splitIf[String](_.isEmpty, input)
        maybeRules = parse(rulesLines)
        exitcode <- Util.executeEither {
            maybeRules.map { implicit rules =>
                messages.count(message => rules(0).recognize(message).isFullMatch)
            }
        }
    } yield ExitCode.Error
}

object Part2 extends IOApp {
    import Day19._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day19/input.txt")
        Seq(rulesLines, messages) = Util.splitIf[String](_.isEmpty, input)
        maybeRules = parse(rulesLines)
        exitcode <- Util.executeEither {
            maybeRules.map { rules =>
                val updated8 = Alternatives(Seq(
                    Sequence(Seq(Reference(42), Reference(8))),
                    Reference(42)
                ))
                val updated11 = Alternatives(Seq(
                    Sequence(Seq(Reference(42), Reference(11), Reference(31))),
                    Sequence(Seq(Reference(42), Reference(31)))
                ))
                implicit val updatedRules = rules.updated(8, updated8)
                    .updated(11, updated11)
                messages.count(message => rules(0).recognize(message).isFullMatch)
            }
        }
    } yield ExitCode.Error
}
