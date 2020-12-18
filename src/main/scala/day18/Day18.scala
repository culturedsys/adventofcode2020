package day18

import day12.L
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util

sealed trait Token
final case class Val(value: Long) extends Token
case object LParen extends Token
case object RParen extends Token
sealed trait Op extends Token 
case object Plus extends Op
case object Mul extends Op

final case class State(acc: Long, op: Option[Op])

object Day18 {

    def evaluate: (Op, Long, Long) => Long = {
        case (Plus, left, right) => left + right
        case (Mul, left, right) => left * right
    }

    def evaluate(expr: Seq[Token]): Long = expr.foldLeft(Seq(State(0, None))) { (states, next) =>
        val State(acc, op) = states.head
        next match {
            case Val(v) => State(op.map(evaluate(_, acc, v)).getOrElse(v), None) +: states.tail
            case op : Op => State(acc, Some(op)) +: states.tail
            case LParen => State(0, None) +: states
            case RParen => {
                val State(oldAcc, oldOp) = states.tail.head
                State(oldOp.map(evaluate(_, oldAcc, acc)).getOrElse(acc), None) +: states.tail.tail
            }
        }
    }.head.acc

    val digit = raw"([0-9])".r
    
    def tokenize(input: Iterator[Char]): Seq[Token] = input.foldLeft(Seq[Token]()) { (acc, next) =>
        next match {
            case '+' => Plus +: acc
            case '*' => Mul +: acc
            case '(' => LParen +: acc
            case ')' => RParen +: acc
            case digit(n) => {
                acc.headOption match {
                    case Some(Val(o)) => Val(o * 10 + (n - '0')) +: acc.tail
                    case Some(_) => Val(n - '0') +: acc
                    case None => Val(n - '0') +: acc
                }
            }
            case _ => acc
        }
    }.reverse
}

object Part1 extends IOApp {
    import Day18._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day18/input.txt")
        expressions = input.map(line => tokenize(line.iterator))
        exitcode <- Util.execute(Some(expressions.map(evaluate).sum))
    } yield ExitCode.Error
}