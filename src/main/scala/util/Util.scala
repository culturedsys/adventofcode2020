package util

import cats.effect.IO
import cats.effect.ExitCode

object Util {    
    def report: Option[Int] => IO[ExitCode] = {
        case None => IO {
            println("Error")
            ExitCode.Error
        }
        case Some(result) => IO { 
            println(result)
            ExitCode.Success 
        }
    }

}