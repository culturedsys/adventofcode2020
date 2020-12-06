package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Util._

class UtilSpec extends AnyFlatSpec with Matchers {    
    
    "splitIf" should "split if the predicate is true" in {
        splitIf[Char](_ == '.', "12.34.56".iterator) shouldBe Seq("12".toSeq, "34".toSeq, "56".toSeq)
    }

    "splitIf" should "skip multiple matching" in {
        splitIf[Char](_ == '.', "12..34".iterator) shouldBe Seq("12".toSeq, "34".toSeq)
    }
}
