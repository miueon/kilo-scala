package effect

import par.Par
import effect.free.Free

enum Files[A]:
  case ReadLines(file: String) extends Files[List[String]]
  case WriteLines(file: String, lines: List[String]) extends Files[Unit]

  def toPar: Par[A] = this match
    case ReadLines(file) =>
      Par.lazyUnit(scala.io.Source.fromFile(file).getLines().toList)
    case WriteLines(file, lines) =>
      ???

  def toThunk: () => A = ???

object Files:
  def readLines(file: String): Free[Files, List[String]] =
    Free.Suspend(Files.ReadLines(file))

  def writeLines(file: String, lines: List[String]): Free[Files, Unit] =
    Free.Suspend(Files.WriteLines(file, lines))