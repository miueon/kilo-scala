package effect

import cats.Monad
import cats.syntax.all.*
import effect.free.Free
import par.Par
import par.Par.{*, given}

// import gears.async.*

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ForkJoinPool
import scala.io.StdIn.*
import cats.MonadError
// import gears.async.default.given

opaque type IO[A] = Free[Par, A]

object IO:
  def now[A](a: A): IO[A] = Free.Return(a)

  def par[A](pa: Par[A]): IO[A] = Free.Suspend(pa)

  def apply[A](a: => A): IO[A] =
    par(Par.delay(a))

  def async[A](cb: (A => Unit) => Unit): IO[A] =
    fork(par(Par.async(cb)))

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))

  extension [A](ioa: IO[A])
    // def unsafeRunSync(using Async): A =
    //   ioa.run.run
    def unsafeRunSync(pool: ExecutorService): A =
      ioa.run.run(pool)

  given monad: Monad[IO] with
    def pure[A](x: A): IO[A] = IO(x)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => IO(b)
      }
end IO
trait IOApp:
  def main(args: Array[String]): Unit =
    pureMain(args.toList).unsafeRunSync(Executors.newCachedThreadPool())

  def pureMain(args: List[String]): IO[Unit]
