package day19

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day19Spec extends AnyFlatSpec with Matchers {
    implicit val rulesMap = Map[Int, Expression]()

    "Literal" should "recognize" in {
        Literal('a').recognize("ab".toSeq) shouldBe Match(Seq(Seq('b')))
    }

    "Literal" should "include expected on miss" in {
        Literal('a').recognize("b".toSeq) shouldBe Miss(Seq('a'))
    }

    "Sequence" should "recognize" in {
        Sequence(Seq(Literal('a'), Literal('b'))).recognize("abc".toSeq) shouldBe Match(Seq(Seq('c')))
    }

    "Sequence" should "include expected on miss" in {
        Sequence(Seq(Literal('a'))).recognize("b".toSeq) shouldBe Miss(Seq('a'))
    }

    "Alternative" should "recognise" in {
       Alternatives(Seq(Literal('a'), Literal('b'))).recognize("bc".toSeq) shouldBe Match(Seq(Seq('c')))
    }

    "Alternative" should "recognize non-deterministic" in {
        // (ab | a) b matches ab
        val result = Sequence(Seq(
            Alternatives(Seq(
                Sequence(Seq(Literal('a'), Literal('b'))),
                Sequence(Seq(Literal('a')))
            )),
            Literal('b')
        )).recognize("ab".toString()) shouldBe 'isFullMatch
    }

    "Alternative" should "include all expected on miss" in {
        Alternatives(Seq(Literal('a'), Literal('b'))).recognize("c".toSeq) shouldBe Miss(Seq('a', 'b'))
    }

    "Reference" should "recognise" in {
        implicit val rulesMap = Map(0 -> Literal('a'))
        Reference(0).recognize("a".toSeq) shouldBe 'isFullMatch
    }
}
