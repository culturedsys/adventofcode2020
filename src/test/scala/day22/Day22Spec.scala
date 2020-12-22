package day22

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day22Spec extends AnyFlatSpec with Matchers {
    import Day22._ 

    "play" should "return the correct winning deck" in {
        play(Vector(9, 2, 6, 3, 1), Vector(5, 8, 4, 7, 10)) shouldBe
            Vector(3, 2, 10, 6, 8, 5, 9, 4, 7, 1)
    }  

    "score" should "return the correct score" in {
        score(Vector(3, 2, 10, 6, 8, 5, 9, 4, 7, 1)) shouldBe 306
    }

    "playRecursive" should "return the correct winning deck" in {
        playRecursive(Vector(9, 2, 6, 3, 1), Vector(5, 8, 4, 7, 10)) shouldBe
            Vector(7, 5, 6, 2, 4, 1, 10, 8, 9, 3)        
    }
}
