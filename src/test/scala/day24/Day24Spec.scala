package day24

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day24Spec extends AnyFlatSpec with Matchers {
    import Day24._ 
    
    "followPath" should "return to origin" in {
        followPath(Seq(NW, W, SW, E, E)) shouldBe (0, 0)
    }

    "followPath" should "end adjecent to origin" in {
        followPath(Seq(E, SE, W)) shouldBe (0, 1)
    }

    "parse" should "parse path" in {
        parse("nwwswee") shouldBe Seq(NW, W, SW, E, E)
    }
}