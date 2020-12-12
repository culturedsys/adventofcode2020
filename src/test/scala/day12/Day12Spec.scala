package day12

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {
    import Day12._

    "rotateWaypoint" should "give the correct answer for 90 degrees" in {
        val result = Boat(0, 0, 0, waypointx = 10, waypointy = 1).rotateWaypoint(90) 
        
        result.waypointx shouldBe (-1.0 +- 0.01)
        result.waypointy shouldBe (10.0 +- 0.01)
    }

    "rotateWaypoint" should "give the correct answer for -90 degrees" in {
        val result = Boat(0, 0, 0, waypointx = 10, waypointy = 1).rotateWaypoint(-90) 
        
        result.waypointx shouldBe (1.0 +- 0.01)
        result.waypointy shouldBe (-10.0 +- 0.01)
    }

    "followPathWithWaypoint" should "give the correct answer for example" in {
        val example = Seq(
            F(10), N(3), F(7), R(90), F(11)
        )

        val result = followPathWithWaypoint(example)

        result.x shouldBe 214
        result.y shouldBe -72
    }
}
