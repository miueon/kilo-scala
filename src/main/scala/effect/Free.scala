package effect.free

import effect.*
import cats.Monad
import cats.syntax.all.*
import par.Par

import java.util.concurrent.Executor
import java.util.concurrent.Executors
import java.util.concurrent.ForkJoinPool
import scala.util.Try


enum Free[+F[_], A]:
  case Return(a: A) extends Free[Nothing, A]
  case Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case FlatMap[F[_], A, B](
      s: Free[F, A],
      f: A => Free[F, B]
  ) extends Free[F, B]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2, B]): Free[F2, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Return(f(a)))

  def union[G[_]]: Free[[x] =>> F[x] | G[x], A] = this
  def covary[F2[x] >: F[x]]: Free[F2, A] = this

  def run[F2[x] >: F[x]](using F: Monad[F2]): F2[A] = step match
    case Return(a)   => F.pure(a)
    case Suspend(fa) => fa
    case FlatMap(Suspend(fa), f) =>
      fa.asInstanceOf[F2[A]]
        .flatMap(a => f.asInstanceOf[A => Free[F2, A]](a).run)
    case FlatMap(_, _) =>
      sys.error("Impossible, since `step` eliminates these cases")

  @annotation.tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(x, f), g) =>
      x.flatMap(a => f(a).flatMap(y => g(y).covary[F])).step
    case FlatMap(Return(x), f) => f(x).step
    case _                     => this

  def runFree[G[_]](t: F ~> G)(using G: Monad[G]): G[A] =
    step match
      case Return(a)  => G.pure(a)
      case Suspend(s) => t(s)
      case FlatMap(x, f) =>
        x match
          case Suspend(resume) =>
            t(resume).flatMap(a => f(a).covary[F].runFree[G](t))
          case _ => sys.error("Impossible, since step eliminates these cases")

  def translate[G[_]](fToG: F ~> G): Free[G, A] =
    runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))
    // runFree returns `GG` here is Free[G, A]

object Free:
  given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa.flatMap(f)
    def pure[A](x: A): Free[F, A] = Return(x)
    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => Return(b)
      }