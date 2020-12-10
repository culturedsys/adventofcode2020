package util

import cats.effect.IO
import cats.effect.ExitCode
import cats.Show
import cats.syntax._
import scala.io.Source
import cats.effect.Clock
import java.util.concurrent.TimeUnit

object Util {    
    def report[T: Show](result: Either[Any, T]): IO[ExitCode] = result match {
        case Left(error) => IO {
            println(error)
            ExitCode.Error
        }
        case Right(result) => IO {
            println(implicitly[Show[T]].show(result))
            ExitCode.Success 
        }
    }

    def report[T: Show](result: Option[T]): IO[ExitCode] = report(result.toRight("Error"))

    def execute[A: Show](f: => Option[A])(implicit clock: Clock[IO]): IO[ExitCode] = for {
        start <- clock.monotonic(TimeUnit.MILLISECONDS)
        result = f
        end <- clock.monotonic(TimeUnit.MILLISECONDS)
        _ <- IO { println(s"Time: ${end - start}") }
        exitcode <- report(result)
    } yield exitcode

    def splitIf[T](pred: T => Boolean, source: Iterator[T]): Seq[Seq[T]] = {
        def go(result: Seq[Seq[T]], partial: Seq[T], remaining: Seq[T]): Seq[Seq[T]] = {
            remaining match {
                case Seq() => result :+ partial
                case head +: tail => 
                    if (pred(head))
                        if (partial.isEmpty) 
                            go(result, partial, tail)
                        else 
                            go(result :+ partial, Seq[T](), tail)
                    else
                        go(result, partial :+ head, tail)
            }
        }

        go(Seq[Seq[T]](), Seq[T](), source.toSeq)
    }

    def readInput(path: String) = IO {
        Source.fromResource(path).getLines()
    }
}