import cats.Monad
import cats.MonadThrow
import cats.data.Kleisli
import cats.data.State
import cats.syntax.all.*
import effect.*
import effect.pull.Stream
import rawmode.all.*

import java.nio.charset.Charset
import java.util.concurrent.Executors
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
import scala.util.Try
import rawmode.all.{disableRawMode as resetRawMode, enableRawMode as setRawMode}
import cats.Eval
import rawmode.*

// TODO write resource on top of Stream resource.
// TODO write resource eff
// TODO
// val program: Eff[AppStack, Unit] =
//   ???
// bracket(TermIOS.enableRawMode)(t =>
//   for
//     // _ <- enableRawMode[AppStack]
//     str <- testGetLine[AppStack]
//     _ <- testPrintLine[AppStack](str)
//   // _ <- disableRawMode[AppStack]
//   yield ()
// )(TermIOS.disableRawMode(_))

// trait AlgInterp[F[_]: MonadThrow]:
//   def enableRawMode: F[Unit]
//   def readLine(): F[Option[String]]
//   def printLine(str: String): F[Unit]
//   def test(): F[Unit]

// type Alg[F[_], A] = Kleisli[F, AlgInterp[F], A]

// def readLine[F[_]: Monad]: Alg[F, Option[String]] = Kleisli(_.readLine())

// def printLine[F[_]: Monad](str: String): Alg[F, Unit] = Kleisli(
//   _.printLine(str)
// )

// def enableRawMode[F[_]: Monad]: Alg[F, Unit] = Kleisli(_.enableRawMode)

// def test[F[_]: Monad]: Alg[F, Unit] =
//   // Kleisli(_.test())
//   for
//     env <- Kleisli.ask[F, AlgInterp[F]]
//     res <- Kleisli.liftF(env.test())
//   yield res
// def readCLine() =
//   val buffer = stackalloc[Byte](1024)
//   val line = stdio.fgets(buffer, 1024, stdio.stdin)
//   fromCString(line)
// val interp = new AlgInterp[Task]:
//   def printLine(str: String): Task[Unit] =
//     // var cstr = Zone(toCString(str))
//     Task
//       .delay {
//         stdio.printf(c"%s\n", Zone(toCString(str)))
//         // throw java.lang.RuntimeException("test")
//       }
//       .handleError(e => println(e.getMessage))
//       .void
//     // Zone {
//     //   IO.fork(stdio.printf(c"%s\n", toCString(str)))
//     // }
//     // IO.forkUnit(println(str))
//   def readLine(): Task[Option[String]] =
//     // IO(Try(readCLine()).toOption)
//     Task.forkUnit(readCLine().some)

//   def test(): Task[Unit] =
//     val c = scala.io.StdIn.readChar()
//     if c != 'q' then Task(println((c).toChar)) >> test()
//     else Task.unit

//   import TerminOSOps.*
//   def enableRawMode: Task[Unit] =
//     Task
//       .now {

//       }
//       .flatMap(_ => test())

// def program[F[_]: Monad]: Alg[F, Unit] =
//   for
//     _ <- printLine("What's your name?")
//     _ <- enableRawMode
//     _ <- test
//   // name <- readLine
//   // _ <- printLine(s"Hello $name")
//   yield ()


object Main extends IOApp:
  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        Task {
          val a = scala.io.StdIn.readLine()
          println(a)
        }
      )
      .asIO
      .void
  end pureMain
end Main
