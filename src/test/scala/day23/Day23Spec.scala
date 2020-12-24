package day23

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Spec extends AnyFlatSpec with Matchers {
    import Day23._

    "rotate" should "rotate the sequence" in {
        rotate(Vector(1, 2, 3, 4)) shouldBe Seq(2, 3, 4, 1)
    }

    "insertAfter" should "insert after" in {
        insertAfter(Vector(1, 2, 3), 2, Seq(5, 6)) shouldBe Vector(1, 2, 5, 6, 3)            
    }

    "insertAfter" should "insert when target is end" in {
        insertAfter(Vector(1, 2, 3), 3, Seq(5, 6)) shouldBe Vector(1, 2, 3, 5, 6)            
    }

    "remove" should "remove n after head" in {
        remove(Vector(1, 2, 3, 4, 5), 3) shouldBe (Seq(2, 3, 4), Vector(1, 5))
    }

    "move" should "complete move" in {
        move(Vector(3, 8, 9, 1, 2, 5, 4, 6, 7)) shouldBe Vector(2, 8, 9, 1, 5, 4, 6, 7, 3)
    }

    "move" should "complete move requiring wrap" in {
        move(Vector(5, 4, 6, 7, 8, 9, 1, 3, 2)) shouldBe Vector(8, 9, 1, 3, 4, 6, 7, 2, 5)
    }
}
