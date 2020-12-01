package day1

import org.scalatest._
import flatspec._
import matchers._

class Day1Spec extends AnyFlatSpec with should.Matchers {
    val examples = Seq(1721,
        979,
        366,
        299,
        675,
        1456)

    "findPairSummingTo" should "give the correct answer for examples" in {
        Day1.findPairSummingTo(examples.toSet, 2020).map {        
            case (x, y) => x * y
        } shouldBe Some(514579)
    }

    "findTripleSummingTo" should "give the correct answer for examples" in {
        Day1.findTripleSummingTo(examples.toSet, 2020).map {        
            case (x, y, z) => x * y * z
        } shouldBe Some(241861950)
    }
  
}
