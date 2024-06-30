package effect

import cats.Applicative
import cats.Eval
import cats.Traverse
import cats.syntax.functor.toFunctorOps
import cats.~>
import gears.async.Async
import org.atnos.eff.*
import org.atnos.eff.syntax.eff.*

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Either
import scala.util.Failure
import scala.util.Success

object TaskEffect extends TaskEffectCreatation, TaskInterpretation

trait IOTypes:
  type _io[R] = |=[Task, R]
  type _Io[R] = <=[Task, R]

trait TaskEffectCreatation extends IOTypes:
  final def fromTask[R: _io, A](task: Task[A]): Eff[R, A] =
    task.send[R]

  final def taskRaiseError[R: _io, A](t: Throwable): Eff[R, A] =
    Task.raiseError(t).send[R]

  final def taskDelay[R: _io, A](v: => A): Eff[R, A] =
    Task(v).send[R]

  final def taskSuspend[R: _io, A](i: => Task[Eff[R, A]]): Eff[R, A] =
    Task.more(i).send[R].flatten

trait TaskInterpretation extends IOTypes:
  def unsafeRunSync[A](e: Eff[Fx1[Task], A])(cb: Either[Throwable, A] => Unit)(using Async): Unit =
    Eff.detach(e).unsafeRunSync

  import interpret.of
  // given Traverse[[x] =>> Either[Throwable, x]] with
  //   def foldLeft[A, B](fa: Either[Throwable, A], b: B)(f: (B, A) => B): B = ???
  //   def foldRight[A, B](fa: Either[Throwable, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  //   def traverse[G[_]: Applicative, A, B](fa: Either[Throwable, A])(f: A => G[B]): G[Either[Throwable, B]] = ???

  def taskAttempt[R, A](e: Eff[R, A])(using m: MemberInOut[Task, R]): Eff[R, Either[Throwable, A]] =
    import cats.syntax.flatMap.toFlatMapOps
    import cats.syntax.applicative.catsSyntaxApplicativeId
    interpret.interceptNatM[R, Task, [x] =>> Either[Throwable, x], A](
      e,
      new (Task ~> ([x] =>> Task[Either[Throwable, x]])):
        def apply[A](fa: Task[A]): Task[Either[Throwable, A]] =
          fa.attempt.flatMap {
            case Failure(exception) => Left(exception).pure
            case Success(value)     => Right(value).pure
          }
    )

  def memoize[A](key: AnyRef, cache: Cache, t: Task[A]): Task[A] =
    cache
      .get[A](key)
      .fold(t.map { r =>
        cache.put(key, r); r
      })(Task.apply)

  import org.atnos.eff.SequenceCached
  given taskSequenceCached: SequenceCached[Task] with
    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: => Task[X]): Task[X] =
      cache.memo((key, sequenceKey), tx)
    def get[X](cache: Cache, key: AnyRef): Task[Option[X]] = Task(cache.get(key))
    def reset(cache: Cache, key: AnyRef): Task[Unit] =
      Task {
        cache.reset(key)
        var i = 0
        while cache.get((key, i)).isDefined do
          cache.reset((key, i))
          i += 1
      }

  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(using task: Task /= R): Eff[R, A] =
    taskAttempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t)  => Eff.send(taskSequenceCached.reset(cache, key)) >> TaskEffect.taskRaiseError(t)
      case Right(a) => Eff.pure(a)
    }

  def taskMemoized[R, A](key: AnyRef, e: Eff[R, A])(using task: Task /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => taskMemo(key, cache, e))

  def runTaskMemo[R, U, A](
      cache: Cache
  )(effect: Eff[R, A])(using m: Member.Aux[Memoized, R, U], task: Task |= U): Eff[U, A] =
    interpret.translate(effect)(
      new Translate[org.atnos.eff.Memoized, U]:
        def apply[X](kv: Memoized[X]): Eff[U, X] =
          kv match
            case GetCache() =>
              TaskEffect.taskDelay(cache)
            case Store(key, value) =>
              TaskEffect.taskDelay(cache.memo(key, value()))
    )
end TaskInterpretation
