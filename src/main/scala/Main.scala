import effect.*
import java.util.concurrent.Executors
import cats.data.Kleisli
import scala.scalanative.libc.*
import scala.scalanative.unsafe.*
import scala.util.Try
import cats.syntax.all.*
import cats.Monad
import java.nio.charset.Charset
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

trait AlgInterp[F[_]]:
  def readLine(): F[Option[String]]
  def printLine(str: String): F[Unit]

type Alg[F[_], A] = Kleisli[F, AlgInterp[F], A]

def readLine[F[_]: Monad]: Alg[F, Option[String]] = Kleisli(_.readLine())

def printLine[F[_]: Monad](str: String): Alg[F, Unit] = Kleisli(
  _.printLine(str)
)

object Main extends IOApp:
  def readCLine() =
    val buffer = stackalloc[Byte](1024)
    val line = stdio.fgets(buffer, 1024, stdio.stdin)
    fromCString(line)
  val interp = new AlgInterp[IO] {
    def printLine(str: String): IO[Unit] =
      // var cstr = Zone(toCString(str))
      IO.forkUnit(stdio.printf(c"%s\n", Zone(toCString(str))))
      // Zone {
      //   IO.fork(stdio.printf(c"%s\n", toCString(str)))
      // }
      // IO.forkUnit(println(str))
    def readLine(): IO[Option[String]] =
      // IO(Try(readCLine()).toOption)
      IO.forkUnit(readCLine().some)
  }

  def program[F[_]: Monad]: Alg[F, Unit] =
    for
      _ <- printLine("What's your name?")
      name <- readLine
      _ <- printLine(s"Hello $name")
    yield ()
  def pureMain(args: List[String]): IO[Unit] =
    program[IO].run(interp)
