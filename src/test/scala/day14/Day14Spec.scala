package day14

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
    import Day14._
    
    "Mask#applyFixed" should "apply 0 and 1 masks" in {
        Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X").applyFixed(11L) shouldBe 73
    }

    "Mask#applyFloating" should "give correct values" in {
        Mask("000000000000000000000000000000X1001X").applyFloating(42L) shouldBe Set(26, 27, 58, 59)
    }
}
