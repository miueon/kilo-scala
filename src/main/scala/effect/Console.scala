package effect

import par.Par
import scala.util.Try
import effect.free.Free
import scala.scalanative.libc.*
import scala.scalanative.unsafe.*

enum Console[A]:
  case ReadLine extends Console[Option[String]]
  case PrintLine(line: String) extends Console[Unit]

  def readLine() =
    val buffer = stackalloc[Byte](1024)
    val line = stdio.fgets(buffer, 1024, stdio.stdin)
    line.toString

  def toTask: Task[A] = this match
    case ReadLine =>
      Task.delay(Try( readLine()).toOption)
    case PrintLine(line) => ???

  // def toPar: Par[A] = this match
  //   case ReadLine        => Par.lazyUnit(Try(readLine()).toOption)
  //   case PrintLine(line) => Par.lazyUnit(println(line))

  // def toThunk: () => A = this match
  //   case ReadLine        => () => Try(readLine()).toOption
  //   case PrintLine(line) => () => println(line)

object Console:
  def readLn: Free[Console, Option[String]] =
    Free.Suspend(ReadLine)

  def printLn(line: String): Free[Console, Unit] =
    Free.Suspend(PrintLine(line))

  // extension [A](fa: Free[Console, A])
  //   def unsafeRunConsole: A =
  //     fa.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline
