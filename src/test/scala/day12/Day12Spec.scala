package day12

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals

class Day12Spec extends AnyFlatSpec with Matchers {
    import Day12._

    implicit val vectorEquality = new Equality[Vector] {
      override def areEqual(left: Vector, right: Any): Boolean = right match {
          case right: Vector => left.dx === (right.dx +- 0.01) && left.dy === (right.dy +- 0.01)
          case _ => false
        }
    }

    "Vector#angle" should "be correct for all cardinal points" in {
        Vector(1, 0).angle shouldBe 0
        Vector(0, 1).angle shouldBe (math.Pi / 2)
        Vector(-1, 0).angle shouldBe (math.Pi)
        Vector(0, -1).angle shouldBe (-math.Pi / 2)
    }

    "Vector#rotate" should "be correct for 90 degree rotations" in {
        Vector(1, 0).rotate(90.toRadians) should equal (Vector(0, 1))
        Vector(1, 0).rotate(180.toRadians) should equal (Vector(-1, 0))
        Vector(1, 0).rotate(270.toRadians) should equal (Vector(0, -1))
        Vector(1, 0).rotate(360.toRadians) should equal (Vector(1, 0))
    }

    "followPathWithWaypoint" should "give the correct answer for example" in {
        val example = Seq(
            F(10), N(3), F(7), R(90), F(11)
        )

        val result = followPathWithWaypoint(example)

        result.location.x shouldBe (214.0 +- 0.01)
        result.location.y shouldBe (-72.0 +- 0.01)
    }
}
