package day18

import day12.L
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.io.Source
import util.Util
import cats.parse.{Parser => P, Parser1}
import cats.syntax._
import cats.implicits._
import cats.Alternative

sealed trait Token
final case class Val(value: Long) extends Token
case object LParen extends Token
case object RParen extends Token
sealed trait Op extends Token 
case object Plus extends Op
case object Mul extends Op

final case class State(acc: Long, op: Option[Op])

sealed trait Expression
final case class Literal(value: Long) extends Expression
final case class MulExpression(left: Expression, right: Expression) extends Expression
final case class SumExpression(left: Expression, right: Expression) extends Expression

object Parser {
    val whitespace = P.charWhere(_.isWhitespace).rep.void

    lazy val expression: Parser1[Expression] = {
        val multiplication: Parser1[Expression] = for {
            left <- sumExp
            _ <- whitespace ~ P.char('*') ~ whitespace
            right <- expression
        } yield MulExpression(left, right)

        multiplication.backtrack <+> sumExp.backtrack
    }

    lazy val sumExp: Parser1[Expression] = {
        val sum: Parser1[Expression] = for {
            left <- terminal
            _ <- whitespace ~ P.char('+') ~ whitespace
            right <- sumExp
        } yield SumExpression(left, right)

        sum.backtrack <+> terminal.backtrack
    }

    lazy val terminal: Parser1[Expression] = literal.backtrack <+> parenthetical.backtrack

    val literal: Parser1[Expression] = P.charsWhile1(_.isDigit).map(digits => Literal(digits.toLong))
    val parenthetical = for {
        _ <- P.char('(') ~ whitespace
        body <- expression
        _ <- whitespace ~ P.char(')')
    } yield body
}

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

    def evaluate(expression: Expression): Long = expression match {
        case Literal(value) => value
        case MulExpression(left, right) => evaluate(left) * evaluate(right)
        case SumExpression(left, right) => evaluate(left) + evaluate(right)
    }

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
    } yield exitcode
}

object Part2 extends IOApp {
    import Day18._

    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day18/input.txt")
        parsed = input.toSeq.traverse(line => Parser.expression.parseAll(line))
        exitcode <- Util.executeEither {
            parsed.map(_.map(evaluate).sum)
        }
    } yield ExitCode.Error
}