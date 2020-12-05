package day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day4Spec extends AnyFlatSpec with Matchers {
    import Day4._

    "splitIf" should "split if the predicate is true" in {
        splitIf[Char](_ == '.', "12.34.56".iterator) shouldBe Seq("12".toSeq, "34".toSeq, "56".toSeq)
    }

    "splitIf" should "skip multiple matching" in {
        splitIf[Char](_ == '.', "12..34".iterator) shouldBe Seq("12".toSeq, "34".toSeq)
    }

    "parseEntry" should "parse all pairs" in {
        parseEntry("foo:bar baz:buzz") shouldBe Map("foo" -> "bar", "baz" -> "buzz")
    }

    "splitEntries" should "split all entries" in {
        splitEntries("""foo
                       |
                       |bar""".stripMargin) shouldBe Seq("foo", "bar")
    }

    "validateRequiredFields" should "return valid if valid" in {
        validateRequiredFields(Map("ecl" -> "gry",
            "pid" -> "860033327",
            "eyr" -> "2020",
            "hcl" -> "#fffffd",
            "byr" -> "1937",
            "iyr" -> "2017",
            "cid" -> "147",
            "hgt" -> "183cm")) shouldBe 'valid
    }


    "validateRequiredFields" should "return invalid if invalid" in {
        validateRequiredFields(Map("ecl" -> "gry",
            "eyr" -> "2020",
            "hcl" -> "#fffffd",
            "byr" -> "1937",
            "iyr" -> "2017",
            "cid" -> "147",
            "hgt" -> "183cm")) shouldBe 'invalid
    }

    "validateRequiredFields" should "return valid if missing optional" in {
        validateRequiredFields(Map(
            "hcl" -> "#ae17e1",
            "iyr" -> "2013",
            "eyr" -> "2024",
            "ecl" -> "brn", 
            "pid" -> "760753108", 
            "byr" -> "1931",
            "hgt" -> "179cm"
        )) shouldBe 'valid
    }

    "validateValues" should "return valid if valid" in {
        validateValues(Map(
            "pid" -> "087499704",
            "hgt" -> "74in",
            "ecl" -> "grn",
            "iyr" -> "2012", 
            "eyr" -> "2030", 
            "byr" -> "1980",
            "hcl" -> "#623a2f"
        )) shouldBe 'valid
    }
}
