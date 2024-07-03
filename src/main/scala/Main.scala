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
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*
import org.atnos.eff.MemberIn
import rawmode.all.{disableRawMode as resetRawMode, enableRawMode as setRawMode}
import effect.TaskEffect.*
import alleycats.std.set
import cats.Eval

object TerminOSOps:
  extension (t: Ptr[termios.termios])
    def c_iflag: termios.tcflag_t = t._1
    def c_oflag: termios.tcflag_t = t._2
    def c_cflag: termios.tcflag_t = t._3
    def c_lflag: termios.tcflag_t = t._4
    def c_lflag_=(v: termios.tcflag_t): Unit = t._4 = v
    def c_cc: termios.c_cc = t._5
    def c_ispeed: termios.speed_t = t._6
    def c_ospeed: termios.speed_t = t._7

def malloc[A](size: CSize): Ptr[A] = stdlib.malloc(size).asInstanceOf[Ptr[A]]

case class TermIOS(orig: Ptr[termios.termios])

object TermIOS:
  def enableRawMode[R: _safe]: Eff[R, Task[TermIOS]] =
    protect {
      Task {
        val orig = malloc[termios.termios](sizeof[termios.termios])
        setRawMode(orig)
        TermIOS(orig = orig)
      }
    }

  def disableRawMode[R: _safe](t: Task[TermIOS]): Eff[R, Task[Unit]] =
    protect {
      t.flatMap(termios =>
        resetRawMode(termios.orig)
        stdlib.free(termios.orig)
        println("freed")
        Task(())
      )
    }

  def useRawMode[R: _safe]: Eff[R, IO[Unit]] =
    org.atnos.eff.all.eval {
      Eval.later(IO(println("test")))
    }
end TermIOS

type _termState[R] = MemberIn[State[TermIOS, *], R]

def enableRawMode[R: _termState]: Eff[R, Unit] =
  for
    term <- get[R, TermIOS]
    _ <- put {
      setRawMode(term.orig)
      term
    }
  yield ()

def disableRawMode[R: _termState]: Eff[R, Unit] =
  for
    term <- get[R, TermIOS]
    _ <- put {
      resetRawMode(term.orig)
      term
    }
  yield ()

def testGetLine[R: _Task]: Eff[R, String] =
  for result <- fromTask(Task(scala.io.StdIn.readLine()))
  yield result
def testPrintLine[R: _Task](str: String): Eff[R, Unit] =
  fromTask(Task(println(str)))
type AppStack = Fx.fx2[Safe, Task]
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

// def streamTest: IO[Unit] =
//   Stream.fromList(List(1, 2, 3)).toEffect.mapEval(x => Task(println(x))).run.asIO
import org.atnos.eff.all.*
object Main extends IOApp:
  def pureMain(args: List[String]): IO[Unit] =
    // Resource
    //   .make[Task, TermIOS](Task {
    //     val ptr = malloc[termios.termios](sizeof[termios.termios])
    //     setRawMode(ptr)
    //     TermIOS(ptr)
    //   })(t =>
    //     Task {
    //       resetRawMode(t.orig)
    //       stdlib.free(t.orig)
    //       println("freed")
    //     }
    //   )
    //   .use(_ =>
    //     Task {
    //       val a = scala.io.StdIn.readLine()
    //       println(a)
    //     }
    //   ).asIO.void
    // Zone { z ?=>
    IO {
      bracketLast(TermIOS.enableRawMode)(t => TermIOS.useRawMode)(
        TermIOS.disableRawMode(_)
      ).runSafe.run._1 match
        case Left(e)      => IO(println(e.getMessage()))
        case Right(value) => value
    }.flatten
      // }.flatten
  end pureMain
  // }.asIO.void
  // streamTest
  // program[Task].run(interp).asIO
end Main
