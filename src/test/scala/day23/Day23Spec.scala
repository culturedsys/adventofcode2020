package day23

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Spec extends AnyFlatSpec with Matchers {
    import Day23._

    "CircularBuffer#toSeq" should "produce the correct sequence" in {
        CupCircle(1, 2, 3).toSeq shouldBe Seq(1, 2, 3)
    }

    "CircularBuffer#rotate" should "rotate the sequence" in {
        val buffer = CupCircle(1, 2, 3, 4)
        buffer.rotate
        buffer.toSeq shouldBe Seq(2, 3, 4, 1)
    }

    "CircularBuffer#insertAfter" should "insert after" in {
        val buffer = CupCircle(1, 2, 3)
        val (insStart, insEnd, _) = CupCircle.fromIterator(Seq(5, 6).iterator)

        buffer.insertAfter(2, insStart, insEnd)
        
        buffer.toSeq shouldBe Seq(1, 2, 5, 6, 3)            
    }

    "CircularBuffer#insertAfter" should "insert when target is end" in {
        val buffer = CupCircle(1, 2, 3)
        val (insStart, insEnd, _) = CupCircle.fromIterator(Seq(5, 6).iterator)
      
        buffer.insertAfter(3, insStart, insEnd)
        
        buffer.toSeq shouldBe Seq(1, 2, 3, 5, 6)            
    }

    "remove" should "remove n after head" in {
        val buffer = CupCircle(1, 2, 3, 4, 5)
        
        buffer.remove(3)
        
        buffer.toSeq shouldBe Seq(1, 5)
    }

    "move" should "complete move" in {
        val buffer = CupCircle(3, 8, 9, 1, 2, 5, 4, 6, 7)
        
        move(buffer, 1, 9) 
        
        buffer.toSeq shouldBe Seq(2, 8, 9, 1, 5, 4, 6, 7, 3)
    }

    "move" should "complete move requiring wrap" in {
        val buffer = CupCircle(5, 4, 6, 7, 8, 9, 1, 3, 2)

        move(buffer, 1, 9) 
        
        buffer.toSeq shouldBe Vector(8, 9, 1, 3, 4, 6, 7, 2, 5)
    }
}
