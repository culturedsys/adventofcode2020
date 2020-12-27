package day25

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day25 {
    def findLoopSize(subjectNumber: Int, prime: Int, publicKey: Int): Int = 
        Iterator.iterate(1L)(value => (value.toLong * subjectNumber) % prime)
            .zipWithIndex
            .dropWhile(_._1 != publicKey)
            .buffered.head._2

    def deriveKey(subjectNumber: Int, prime: Int, loopSize: Int): Long = 
        BigInt(subjectNumber).modPow(loopSize, prime).toLong
}

object Part1 extends IOApp {
    import Day25._

    override def run(args: List[String]): IO[ExitCode] = {
        val cardPk = 10441485
        val doorPk = 1004920
        Util.execute {
            val cardLoopSize = findLoopSize(7, 20201227, cardPk)
            val encryptionKey = deriveKey(doorPk, 20201227, cardLoopSize)
            Some(encryptionKey)
        }
    }

}
