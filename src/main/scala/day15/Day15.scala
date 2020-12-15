package day15

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day15 {
    def iterateResults(startingState: (Map[Int, Int], Int, Int)) = 
        Iterator.iterate(startingState) {
            case (history, lastNumber, turn) => {
                val lastSeen = history.get(lastNumber)
                val nextNumber = if (lastSeen.isEmpty) 
                    0
                else
                    turn - lastSeen.get
                (history.updated(lastNumber, turn), nextNumber, turn + 1)
            }
        }

    def stateFromNumbers(numbers: Seq[Int]) = {
        val countedNumbers = numbers.zipWithIndex
        val last = countedNumbers.last
        val history = Map.from(countedNumbers.init)
        (history, last._1, last._2)
    }

    def nthResult(starting: Seq[Int], n: Int) =
        iterateResults(stateFromNumbers(starting))
            .dropWhile(_._3 < n - 1).buffered.head._2
}

object Part1 extends IOApp {
    import Day15._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        exitcode <- Util.execute(Some(nthResult(Seq(7,14,0,17,11,1,2), 2020)))
    } yield exitcode
}

object Part2 extends IOApp {
    import Day15._ 

    override def run(args: List[String]): IO[ExitCode] = for {
        exitcode <- Util.execute(Some(nthResult(Seq(7,14,0,17,11,1,2), 30000000)))
    } yield exitcode
}