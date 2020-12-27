package day25

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util

object Day25 {
    /**
     * Find x where alpha ^ x = beta (mod modulus)
     * 
     * Works by finding i, m, j such that alpha ^ (i * m + j) = beta, which is equivalent to
     * alpha ^ j = beta * (alpha ^ -m) ^ i
     * 
    */
    def discreteLog(alpha: Int, beta: Int, modulus: Int): Int = {
        // Fix m as sqrt(modulus)
        val m = math.ceil(math.sqrt(modulus)).toInt

        // Calculate values of alpha ^ j where 0 <= j < m
        val alphaPrime = BigInt(alpha)
        val alphaToJMap = (0 until m).map(j => (alphaPrime.modPow(j, modulus).toLong, j)).toMap

        val alphaToMinusM = alphaPrime.modPow(-m, modulus)

        // Find value of i, j where beta * (alpha ^ -m) ^ i = alpha ^ j
        val (i, j) = (0 until m).map { i =>
            alphaToJMap.get(((beta * alphaToMinusM.modPow(i, modulus)) % modulus).toLong)
                .map(j => (i, j))
        }.flatten.head

        // Calculate x given m, i, j
        i * m + j
    }

    def findLoopSize(subjectNumber: Int, modulus: Int, publicKey: Int): Int = 
        discreteLog(subjectNumber, publicKey, modulus)

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
