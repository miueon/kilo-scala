package par.gear

import gears.async.*
import gears.async.ScalaConverters.*
import scala.util.Try
import scala.concurrent.Future as SFuture
import cats.MonadThrow
import scala.util.Failure
import scala.util.Success
import gears.async.Async.await

opaque type Par[+A] = Async => Future[A]

object Par:
  given parMonadThrow: MonadThrow[Par] with
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = fa.flatMap(f)
    def handleErrorWith[A](fa: Par[A])(f: Throwable => Par[A]): Par[A] = async =>
      fa(async).awaitResult(using async) match
        case Failure(exception) => f(exception)(async)
        case Success(value)     => Future.now(Success(value))
    def pure[A](x: A): Par[A] = Par.unit(x)
    def raiseError[A](e: Throwable): Par[A] = async => Future.now(Failure(e))
    def tailRecM[A, B](a: A)(f: A => Par[Either[A, B]]): Par[B] =
      f(a).flatMap {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => Par.unit(value)
      }

  def unit[A](a: A): Par[A] =
    _ => Future.now(Try(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = async => Async.group(inner ?=> a(inner))(using async)

  def delay[A](a: => A): Par[A] = _ => Future.now(Try(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def async[A](f: (A => Unit) => Unit): Par[A] = async =>
    Future.withResolver { resolver =>
      f(result => resolver.resolve(result))
    }

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(IndexedSeq(_))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    val fbs = ps.map(asyncF(f))
    sequence(fbs)

  def join[A](ppa: Par[Par[A]]): Par[A] =
    ppa.flatMap(identity)

  extension [A](pa: Par[A])
    def map2[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
      async =>
        Async.group { inner ?=>
          val fa = Async.group(inner ?=> pa(inner))
          val fb = Async.group(inner ?=> p2(inner))
          val a = fa.await
          val b = fb.await
          Future(f(a, b))
        }(using async)

    def map[B](f: A => B): Par[B] = pa.map2(unit(()))((a, _) => f(a))

    def flatMap[B](f: A => Par[B]): Par[B] = fork(async =>
      val a = pa(async).await(using async)
      f(a)(async)
    )

    def run(using async: Async): A =
      pa(async).await
  end extension
end Par
