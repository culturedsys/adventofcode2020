package day22

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day22 {
    def play(deck1: Vector[Int], deck2: Vector[Int]): Vector[Int] = (deck1, deck2) match {
        case (Vector(), deck2) => deck2
        case (deck1, Vector()) => deck1
        case ((card1 +: rest1), (card2 +: rest2)) => {
            val (deck1, deck2) = if (card1 < card2)
                (rest1, rest2 :+ card2 :+ card1)
            else
                (rest1 :+ card1 :+ card2, rest2)            

            play(deck1, deck2)
        }
    }

    def score(deck: Vector[Int]): Long =
        deck.reverse.zipWithIndex.map { case (value, multiplier) => value * (multiplier + 1)}.sum

    def playRecursive(deck1: Vector[Int], deck2: Vector[Int]): Vector[Int] = {
        def playTurn(deck1: Vector[Int], deck2: Vector[Int]) = {
            val (card1, rest1) = (deck1.head, deck1.tail)
            val (card2, rest2) = (deck2.head, deck2.tail)

            val winner = 
                if (card1 <= rest1.length && card2 <= rest2.length)
                    go(rest1.take(card1), rest2.take(card2), Set(), Set())._1
                else if (card1 > card2)
                    1
                else
                    2

            if (winner == 1)
                (rest1 :+ card1 :+ card2, rest2)
            else
                (rest1, rest2 :+ card2 :+ card1)
        }

        def go(deck1: Vector[Int], deck2: Vector[Int], deck1History: Set[Vector[Int]], deck2History: Set[Vector[Int]]): 
                (Int, Vector[Int]) = (deck1, deck2) match {
            case (deck1, Vector()) => (1, deck1)
            case (Vector(), deck2) => (2, deck2)
            case (deck1, deck2) => 
                if (deck1History.contains(deck1) && deck2History.contains(deck2)) 
                    (1, deck1)
                else {
                    val (newDeck1, newDeck2) = playTurn(deck1, deck2)
                    go(newDeck1, newDeck2, deck1History + deck1, deck2History + deck2)
                }
        }

        go(deck1, deck2, Set(), Set())._2
    }

    def parse(input: Seq[String]): Vector[Int] = 
        input.tail.map(_.toInt).toVector
}

object Part1 extends IOApp {
    import Day22._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day22/input.txt")
        Seq(player1, player2) = Util.splitIf[String](_.isEmpty, input).map(parse)
        exitcode <- Util.execute {
            val winning = play(player1, player2)
            Some(score(winning))
        }
    } yield ExitCode.Error   
}

object Part2 extends IOApp {
    import Day22._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day22/input.txt")
        Seq(player1, player2) = Util.splitIf[String](_.isEmpty, input).map(parse)
        exitcode <- Util.execute {
            val winning = playRecursive(player1, player2)
            Some(score(winning))
        }
    } yield ExitCode.Error
}