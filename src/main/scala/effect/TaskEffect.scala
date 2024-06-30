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

trait TaskTypes:
  type _task[R] = |=[Task, R]
  type _Task[R] = <=[Task, R]

trait TaskEffectCreatation extends TaskTypes:
  final def fromTask[R: _task, A](task: Task[A]): Eff[R, A] =
    task.send[R]

  final def taskRaiseError[R: _task, A](t: Throwable): Eff[R, A] =
    Task.raiseError(t).send[R]

  final def taskDelay[R: _task, A](v: => A): Eff[R, A] =
    Task(v).send[R]

  final def taskSuspend[R: _task, A](i: => Task[Eff[R, A]]): Eff[R, A] =
    Task.more(i).send[R].flatten

object TaskInterpretation extends TaskInterpretation
trait TaskInterpretation extends TaskTypes:
  def unsafeRunSync[A](e: Eff[Fx1[Task], A])(using Async): A =
    Eff.detach(e).unsafeRunSync

  def toTask[A](e: Eff[Fx1[Task], A]): Task[A] = Eff.detach(e)

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(using m: MemberInOut[Task, R]): Eff[R, Either[Throwable, A]] =
    import cats.syntax.flatMap.toFlatMapOps
    import cats.syntax.applicative.catsSyntaxApplicativeId
    interpret.interceptNatM[R, Task, Either[Throwable, *], A](
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
