package util

import cats.effect.IO
import cats.effect.ExitCode
import cats.Show
import cats.syntax._

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
}