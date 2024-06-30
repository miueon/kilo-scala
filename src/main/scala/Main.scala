import cats.Monad
import cats.MonadThrow
import cats.data.Kleisli
import cats.syntax.all.*
import effect.*
import effect.pull.Stream

import java.nio.charset.Charset
import java.util.concurrent.Executors
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*

import scala.util.Try
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
case class Player(name: String, score: Int)

// def winnerMsg(p: Option[Player]): String = p
//   .map { case Player(name, _) =>
//     s"$name is the winner!"
//   }
//   .getOrElse("It's a draw")

// def winner(p1: Player, p2: Player): Option[Player] =
//   if p1.score > p2.score then Some(p1)
//   else if p1.score < p2.score then Some(p2)
//   else None

// def contest(p1: Player, p2: Player): IO[Unit] =
//   PrintLine(winnerMsg(winner(p1, p2)))
// object Main extends IOApp {
//   def pureMain(args: List[String]): IO[Unit] =
//     contest(Player("Alice", 60), Player("Bob", 50))
// }

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

trait AlgInterp[F[_]: MonadThrow]:
  def enableRawMode: F[Unit]
  def readLine(): F[Option[String]]
  def printLine(str: String): F[Unit]
  def test(): F[Unit]

type Alg[F[_], A] = Kleisli[F, AlgInterp[F], A]

def readLine[F[_]: Monad]: Alg[F, Option[String]] = Kleisli(_.readLine())

def printLine[F[_]: Monad](str: String): Alg[F, Unit] = Kleisli(
  _.printLine(str)
)

def enableRawMode[F[_]: Monad]: Alg[F, Unit] = Kleisli(_.enableRawMode)

def test[F[_]: Monad]: Alg[F, Unit] = 
  // Kleisli(_.test())
  for 
    env <- Kleisli.ask[F, AlgInterp[F]]
    res <- Kleisli.liftF(env.test())
  yield res

object Main extends IOApp:
  def readCLine() =
    val buffer = stackalloc[Byte](1024)
    val line = stdio.fgets(buffer, 1024, stdio.stdin)
    fromCString(line)
  val interp = new AlgInterp[Task]:
    def printLine(str: String): Task[Unit] =
      // var cstr = Zone(toCString(str))
      Task
        .delay {
          stdio.printf(c"%s\n", Zone(toCString(str)))
          // throw java.lang.RuntimeException("test")
        }
        .handleError(e => println(e.getMessage))
        .void
      // Zone {
      //   IO.fork(stdio.printf(c"%s\n", toCString(str)))
      // }
      // IO.forkUnit(println(str))
    def readLine(): Task[Option[String]] =
      // IO(Try(readCLine()).toOption)
      Task.forkUnit(readCLine().some)

    def test(): Task[Unit] =
      val c = scala.io.StdIn.readChar()
      if c != 'q' then Task(println((c).toChar)) >> test()
      else Task.unit

    import TerminOSOps.*
    def enableRawMode: Task[Unit] =
      Task
        .now {

        }
        .flatMap(_ => test())

  def program[F[_]: Monad]: Alg[F, Unit] =
    for
      _ <- printLine("What's your name?")
      _ <- enableRawMode
      _ <- test
    // name <- readLine
    // _ <- printLine(s"Hello $name")
    yield ()

  def streamTest: IO[Unit] =
    Stream.fromList(List(1, 2, 3)).toEffect.mapEval(x => Task(println(x))).run.asIO
  def pureMain(args: List[String]): IO[Unit] =
    // streamTest
    program[Task].run(interp).asIO
end Main
