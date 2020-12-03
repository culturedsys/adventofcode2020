package util

import cats.effect.IO
import cats.effect.ExitCode
import cats.Show
import cats.syntax._

object Util {    
    def report[T: Show](result: Option[T]): IO[ExitCode] = result match {
        case None => IO {
            println("Error")
            ExitCode.Error
        }
        case Some(result) => IO {
            println(implicitly[Show[T]].show(result))
            ExitCode.Success 
        }
    }

}