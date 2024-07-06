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
import scala.scalanative.libc.{errno as libcErrno, *}
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
import scala.util.Try
import rawmode.all.{disableRawMode as resetRawMode, enableRawMode as setRawMode}
import cats.Eval
import rawmode.*
import scala.scalanative.posix.cpio
import scala.scalanative.posix.errno
import cats.data.EitherT
import util.Utils.*
import cats.data.StateT

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
case class EditorConfig(a: Int)

type EditorConfigState[F[_]] = StateT[F, EditorConfig, Unit]

object Main extends IOApp:
  inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
  inline def resetCursorAndScreen(): Task[Unit] =
    Task(unistd.write(unistd.STDOUT_FILENO, c"\x1b[2J", 4.toUInt)) // reset screen
      >> Task(unistd.write(unistd.STDOUT_FILENO, c"\x1b[H", 3.toUInt)) // reset cursor, [x,xH

  def editorReadKey(ref: Ref[Task, Ptr[CChar]]): Task[Unit] =
    for
      cPtr <- ref.get
      nread <- Task.delay(unistd.read(unistd.STDIN_FILENO, cPtr, UInt.valueOf(1)))
      _ <-
        if nread != 1 then editorReadKey(ref)
        else if nread == -1 && errno.errno != errno.EAGAIN then Task.raiseError(new Exception("read"))
        else Task.unit
    yield ()

  def editorProcessKeypress(): EitherT[Task, Int, Unit] =
    val result: Task[Either[Int, Unit]] = for
      cPtrRef <- Task(Ref[Task, Ptr[CChar]](malloc[CChar]))
      _ <- editorReadKey(cPtrRef)
      // r <- Task(println("keypressed")) >> Task(Left(0))
      cPtr <- cPtrRef.get
      // _ <- Task(println(s"keypressed: ${!cPtr}"))
      r <- !cPtr match
        case a if a == ctrlKey('q') => resetCursorAndScreen() >> Task(Left(0))
        case _                      => Task(Right(()))
    yield r
    EitherT(result)

  def editorDrawRows(): Task[Unit] =
    Stream.eval(Task(unistd.write(unistd.STDOUT_FILENO, c"~\r\n", 3.toUInt)).void).repeat.take(24).run

  def editorRefreshScreen(): Task[Unit] =
    for
      _ <- resetCursorAndScreen()
      _ <- editorDrawRows()
      _ <- Task(unistd.write(unistd.STDOUT_FILENO, c"\x1b[H", 3.toUInt))
    yield ()

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        def go(): Task[Unit] =
          for
            _ <- editorRefreshScreen()
            result <- editorProcessKeypress().isLeft
            _ <- if result then Task.unit else go()
          yield ()
        go()
      )
      .handleErrorWith(e =>
        resetCursorAndScreen() >> Task.apply(
          Zone(stdio.printf(c"%s\n%s\n", toCString(e.getMessage), string.strerror(errno.errno)))
        )
      )
      .asIO
      .void
  end pureMain
end Main
