package day23

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day23 {
    def rotate(v: Vector[Int]): Vector[Int] = v match {
        case head +: tail => tail :+ head
        case _ => Vector()
    }

    def insertAfter(v: Vector[Int], target: Int, addition: Seq[Int]): Vector[Int] = {
        val (before, after) = v.span(_ != target)
        (before :+ after.head) ++ addition ++ after.tail 
    }

    def remove(v: Vector[Int], n: Int): (Seq[Int], Vector[Int]) =
        (v.tail.take(n), v.head +: v.tail.drop(n))

    def move(v: Vector[Int]): Vector[Int] = {
        val (picked, remainder) = remove(v, 3)
        val min = v.min
        val max = v.max
        def decrementWrap(candidate: Int) = if (candidate - 1 < min) max else candidate - 1
        val target = Stream.iterate(v.head)(decrementWrap)
            .tail.dropWhile(picked.contains(_)).head
        rotate(insertAfter(remainder, target, picked))
    }

    def formatResult(v: Vector[Int]): String = {
        val (before, after) = v.span(_ != 1)
        (after.tail ++ before).mkString
    }
}

object Part1 extends IOApp {
    import Day23._

    override def run(args: List[String]): IO[ExitCode] = Util.execute {
        val result = Stream.iterate(Vector(1, 2, 3, 4, 8, 7, 5, 9, 6))(move).drop(100).head
        Some(formatResult(result))
    } 
}