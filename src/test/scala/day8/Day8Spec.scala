package day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import javax.swing.plaf.synth.Region

class Day8Spec extends AnyFlatSpec with Matchers {
    import Day8._
    import Instruction._

    "run" should "give correct results for all opcodes" in {
        Day8.run(Vector(
            Nop(0),
            Acc(2),
            Jmp(-2)
        )).take(4) shouldBe Stream(
            Registers(0, 0),
            Registers(pc = 1, acc = 0),
            Registers(pc = 2, acc = 2),
            Registers(pc = 0, acc = 2)
        )
    }

    val example = Vector(
            Nop(0),
            Acc(1),
            Jmp(+4),
            Acc(+3),
            Jmp(-3),
            Acc(-99),
            Acc(+1),
            Jmp(-4),
            Acc(+6)
    )

    "runToRepetition" should "give correct result for example" in {
        runToRepetition(example).last shouldBe Registers(pc = 4, acc = 5)
    }

    "runToExit" should "return None if repeats" in {
        runToExit(example) shouldBe None
    }

    val terminating = Vector(
            Nop(0),
            Acc(1),
            Jmp(+4),
            Acc(+3),
            Jmp(-3),
            Acc(-99),
            Acc(+1),
            Nop(-4),
            Acc(+6)
    )

    "runToExit" should "return the exit registers" in {
        runToExit(terminating).map(_.acc) shouldBe Some(8)
    }

    "findTerminatingPermutation" should "return the correct result in example" in {
        findTerminatingPermutation(example).map(_.acc) shouldBe Some(8)
    }
}
