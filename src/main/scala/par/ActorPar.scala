package par

import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import cats.Monad
import cats.Applicative

opaque type Future[+A] = (A => Unit) => Unit
opaque type Par[+A] = ExecutorService => Future[A]

object Par:
  given parMonad: Monad[Par] with
    def pure[A](x: A): Par[A] = Par.unit(x)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Par[Either[A, B]]): Par[B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => Par.unit(b)
      }

  given parApplicative: Applicative[Par] with
    def ap[A, B](ff: Par[A => B])(fa: Par[A]): Par[B] =
      ff.map2(fa)((f, a) => f(a))
    def pure[A](x: A): Par[A] = lazyUnit(x)
  def unit[A](a: A): Par[A] =
    es => cb => cb(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def delay[A](a: => A): Par[A] =
    es => cb => cb(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => cb => eval(es)(a(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(
      new Callable[Unit]:
        def call(): Unit = r
    )

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def async[A](f: (A => Unit)  => Unit): Par[A] = 
    es => cb => f(cb)

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

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    // es =>
    //   cb =>
    //     cond(es) { b =>
    //       if b then eval(es)(t(es)(cb))
    //       else eval(es)(f(es)(cb))
    //     }
    cond.flatMap(b => if b then t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    // n.map(index => choices(index))
    // es => cb => n(es)(ind => eval(es)(choices(ind % choices.length)(es)(cb)))
    n.flatMap(index => choices(index % choices.length))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    // es => cb => key(es)(k => choices(k)(es)(cb))
    key.flatMap(k => choices(k))

  def join[A](ppa: Par[Par[A]]): Par[A] =
    // es => cb => ppa(es)(pa => eval(es)(pa(es)(cb)))
    ppa.flatMap(identity)

  extension [A](pa: Par[A])
    def run(es: ExecutorService): A =
      val ref = AtomicReference[A]
      val latch = CountDownLatch(1)
      pa(es) { a =>
        ref.set(a); latch.countDown()
      }
      latch.await()
      ref.get

    def chooser[B](choices: A => Par[B]): Par[B] =
      // es => cb => pa(es)(a => eval(es)(choices(a)(es)(cb)))
      pa.flatMap(choices)

    def flatMap[B](f: A => Par[B]): Par[B] =
      fork(es => cb => pa(es)(a => f(a)(es)(cb)))

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def map2[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        cb =>
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es):
            case Left(a) =>
              if br.isDefined then eval(es)(cb(f(a, br.get)))
              else ar = Some(a)

            case Right(b) =>
              if ar.isDefined then eval(es)(cb(f(ar.get, b)))
              else br = Some(b)

          pa(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
  end extension
end Par
