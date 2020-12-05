package day4

import scala.collection.StringOps
import java.lang.invoke.StringConcatFactory
import scala.collection.Factory
import cats.kernel.Monoid
import cats.data.Validated
import cats.syntax._
import cats.implicits._
import cats.data.Validated.Valid
import cats.data.ValidatedNec
import cats.data.NonEmptyChain
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util
import scala.util.matching.Regex

final case class Entry(
    byr: String,
    iyr: String,
    eyr: String,
    hgt: String,
    hcl: String,
    ecl: String,
    pid: String,
    cid: Option[String]
)

sealed trait ValidationError
final case class Missing(name: String) extends ValidationError
final case class Invalid(name: String, message: String) extends ValidationError

object Day4 {
    def splitIf[T](pred: T => Boolean, source: Iterator[T]): Seq[Seq[T]] = {
        def go(result: Seq[Seq[T]], partial: Seq[T], remaining: Seq[T]): Seq[Seq[T]] = {
            remaining match {
                case Seq() => result :+ partial
                case head +: tail => 
                    if (pred(head))
                        if (partial.isEmpty) 
                            go(result, partial, tail)
                        else 
                            go(result :+ partial, Seq[T](), tail)
                    else
                        go(result, partial :+ head, tail)
            }
        }

        go(Seq[Seq[T]](), Seq[T](), source.toSeq)
    }

    def parseEntry(entry: String): Map[String, String] = 
        entry.split(raw"\s+").map { pair => 
            pair.span(_ != ':') match {
                case (key, value) => (key, value.substring(1))
            }
        }.toMap
    
    def splitEntries(entries: String): Seq[String] = {
        splitIf[String](line => line.isEmpty, entries.linesIterator)
            .map(_.mkString( "\n"))
    }

    import Validated.invalidNec

    def require(name: String, entry: Map[String, String]) =
        Validated.fromOption(entry.get(name), NonEmptyChain(Missing(name)))

    def optional(name: String, entry: Map[String, String]) =
        Valid(entry.get(name))

    def validateRequiredFields(entry: Map[String, String]): ValidatedNec[ValidationError, Entry] = {
        (
            require("byr", entry),
            require("iyr", entry),
            require("eyr", entry),
            require("hgt", entry),
            require("hcl", entry),
            require("ecl", entry),
            require("pid", entry),
            optional("cid", entry)
        ).mapN(Entry)
    }

    val yearRegex = raw"([0-9]{4})".r

    def validYear(key: String, min: Int, max: Int, entry: Map[String, String]) =
        require(key, entry).andThen {
            case yearRegex(year) => 
                if (year.toInt >= min && year.toInt <= max) Valid(year) 
                else invalidNec(Invalid(key, "Out of range"))
            case _ => invalidNec(Invalid(key, "Bad year format"))
        }

    val heightRegex = raw"([0-9]+)([a-z]+)".r

    def validHeight(key: String, ranges: Map[String, (Int, Int)], entry: Map[String, String]) = 
        require(key, entry).andThen {
            case full @ heightRegex(value, unit) =>
                ranges.get(unit) match {
                    case Some((min, max)) => 
                        if (value.toInt >= min && value.toInt <= max) Valid(full) 
                        else invalidNec(Invalid(key, "Out of range"))
                   case _ => invalidNec(Invalid(key, "Bad unit"))
                }
            case _ => invalidNec(Invalid(key, "Bad format"))
        }


    def validRegex(key: String, regex: Regex, entry: Map[String, String]) = 
        require(key, entry).andThen {
            case regex(value) => Valid(value)
            case _ => invalidNec(Invalid(key, "No match"))
        }

    def validOneOf(key: String, options: Set[String], entry: Map[String, String]) = 
        require(key, entry)
                .andThen(value => if (options.contains(value)) Valid(value) else invalidNec(Invalid(key, "Not in set")))
 
    def validateValues(entry: Map[String, String]): ValidatedNec[ValidationError, Entry] = {
        (
            validYear("byr", 1920, 2002, entry),
            validYear("iyr", 2010, 2020, entry),
            validYear("eyr", 2020, 2030, entry),
            validHeight("hgt", Map("cm" -> (150, 193), "in" -> (59, 76)), entry),
            validRegex("hcl", raw"(#[0-9a-f]{6})".r, entry),
            validOneOf("ecl", Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth"), entry),
            validRegex("pid", raw"([0-9]{9})".r, entry),
            optional("cid", entry)
        ).mapN(Entry)
    }

    def countValidEntries(entries: Seq[Validated[_, Entry]]): Int = 
        entries.count(_.isValid)
}

object Part1 extends IOApp {
    import Day4._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- IO { Source.fromResource("day4/input.txt").mkString }
        entries = splitEntries(input).map(parseEntry(_))
        result = countValidEntries(entries.map(validateRequiredFields(_)))
        exitcode <- Util.report(Some(result))
    } yield exitcode
}

object Part2 extends IOApp {
    import Day4._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- IO { Source.fromResource("day4/input.txt").mkString }
        entries = splitEntries(input).map(parseEntry(_))
        result = countValidEntries(entries.map(validateValues(_)))
        exitcode <- Util.report(Some(result))
    } yield exitcode
}