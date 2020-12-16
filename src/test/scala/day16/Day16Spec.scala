package day16

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.NonEmptyList

class Day16Spec extends AnyFlatSpec with Matchers {
    import Day16._

    "invalidValues" should "give the correct answer for example" in {
        invalidValues(
            Seq(
                Rule("class", Seq(1 to 3, 5 to 7)),
                Rule("row", Seq(6 to 11, 33 to 44)),
                Rule("seat", Seq(13 to 40, 45 to 50))
            ),
            Seq(
                Ticket(Seq(7,3,47)),
                Ticket(Seq(40,4,50)),
                Ticket(Seq(55,2,20)),
                Ticket(Seq(38,6,12))
            )
        ) shouldBe Seq(4, 55, 12)
    }

    "parse" should "correctly parse a number" in {
        number.parseAll("1") shouldBe Right(1)
    }

    "parse" should "correctly parse a range" in {
        range.parseAll("1-3") shouldBe Right(1 to 3)
    }

    "parse" should "correctly parse ranges" in {
        ranges.parseAll("1-3 or 5-7") shouldBe Right(NonEmptyList.of(1 to 3, 5 to 7))
    }

    "parse" should "correctly parse a rule" in {
        rule.parseAll("class: 1-3 or 5-7") shouldBe Right(Rule("class", Seq(1 to 3, 5 to 7)))        
    }

    "parse" should "correctly parse a ticket" in {
        ticket.parseAll("7,1,14") shouldBe Right(Ticket(Seq(7, 1, 14)))
    }

    "parse" should "correctly parse yourTicketSep" in {
        yourTicketSep.parseAll("your ticket:") shouldBe Right(())
    }

    "parse" should "correctly parse all notes" in {
        val example =
            """class: 1-3 or 5-7
              |row: 6-11 or 33-44
              |seat: 13-40 or 45-50
              |
              |your ticket:
              |7,1,14
              |
              |nearby tickets:
              |7,3,47
              |40,4,50
              |55,2,20
              |38,6,12""".stripMargin

        // example.zipWithIndex.foreach(t => println(s"<${t._1}>, ${t._2}"))

        val result = notes.parseAll(example)
        result.map(_._1) shouldBe Right(
            Seq(
                Rule("class", Seq(1 to 3, 5 to 7)),
                Rule("row", Seq(6 to 11, 33 to 44)),
                Rule("seat", Seq(13 to 40, 45 to 50))
            ))

        result.map(_._2) shouldBe Right(Ticket(Seq(7, 1, 14)))

        result.map(_._3) shouldBe Right(
            Seq(
                Ticket(Seq(7, 3, 47)),
                Ticket(Seq(40, 4, 50)),
                Ticket(Seq(55, 2, 20)),
                Ticket(Seq(38, 6, 12))
            )
        )
    }
}
