package day23

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class CupCircle(var head: CupCircle.Cup, val labelToCup: Map[Int, CupCircle.Cup]) {
    def toSeq: Seq[Int] = {
        val result = ListBuffer[Int]()
        var pointer = head
        do {
            result.addOne(pointer.label)
            pointer = pointer.next
        } while (pointer != head)

        result.toSeq
    }

    def rotate: Unit = {
        head = head.next
    }

    def find(label: Int): CupCircle.Cup = 
        labelToCup(label)

    def insertAfter(label: Int, first: CupCircle.Cup, last: CupCircle.Cup): Unit = {
        val target = find(label)
        last.next = target.next
        target.next = first
    }

    def remove(n: Int): (CupCircle.Cup, CupCircle.Cup) = {
        var target = head.next
        val result = target

        (0 until n - 1).foreach{ _ => 
            target = target.next
        }

        head.next = target.next
        (result, target)
    }
}

object CupCircle {
    class Cup(var label: Int = 0, var next: Cup = null) {
        def isIn(value: Int, n: Int): Boolean = {
            var target = this
            var i = 0
            while (i < n) {
                if (target.label == value)
                    return true
                target = target.next
                i += 1
            }

            return false
                
        }
    }

    def apply(values: Int*): CupCircle = {
        apply(values.iterator)
    }

    def apply(values: Iterator[Int]): CupCircle = {
        val (head, last, labelToCup) = fromIterator(values)
        last.next = head
        new CupCircle(head, labelToCup)
    }

    def fromIterator(values: Iterator[Int]): (Cup, Cup, Map[Int, Cup]) = {
        val first = new Cup(values.next(), new Cup())
        var last = first
        var  labelToCup = mutable.Map[Int, Cup](first.label -> first)

        while (values.hasNext) {
            last = last.next
            last.label = values.next()
            labelToCup(last.label) = last
            last.next = new Cup()
        }

        (first, last, labelToCup.toMap)
    }
}

object Day23 {
    def move(v: CupCircle, min: Int, max: Int): Unit = {
        val (picked, last) = v.remove(3)
        var target = v.head.label
        do {
            target = if (target - 1 < min) max else target - 1            
        } while (picked.isIn(target, 3))     

        v.insertAfter(target, picked, last)
        v.rotate
    }

    def formatResult(v: Seq[Int]): String = {
        val (before, after) = v.span(_ != 1)
        (after.tail ++ before).mkString
    }
}

object Part1 extends IOApp {
    import Day23._

    override def run(args: List[String]): IO[ExitCode] = for {
        buffer <- IO { CupCircle(1, 2, 3, 4, 8, 7, 5, 9, 6) }        
        exitcode <- Util.execute {
            (0 until 100).foreach { _ => 
                move(buffer, 1, 9)
            }
            Some(formatResult(buffer.toSeq))
        }
    } yield exitcode 
}

object Part2 extends IOApp {
    import Day23._

    override def run(args: List[String]): IO[ExitCode] = {
        val buffer = { 
            val init = Stream(1, 2, 3, 4, 8, 7, 5, 9, 6) ++ Stream.from(10 to 1_000_000)
            CupCircle(init.iterator) 
        }
        
        Util.execute {
            (0 until 10_000_000).foreach { _ => 
                move(buffer, 1, 1_000_000)
            }
            val target = buffer.find(1)
            Some(target.next.label.toLong * target.next.next.label)
        }
    }
}