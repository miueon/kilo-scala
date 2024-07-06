package effect

import gears.async.Async

type Nothing1[A] = Nothing
infix type ~>[F[_], G[_]] = [x] => F[x] => G[x]

final class Id

import org.atnos.eff.*
extension [A](e: Eff[Fx1[Task], A])
  def unsafeRunSync(using Async): A =
    TaskEffect.unsafeRunSync(e)

  def toTask: Task[A] = 
    TaskEffect.toTask(e)

extension [R, A](e: Eff[R, A])
  def taskAttempt(using m: MemberInOut[Task, R]): Eff[R, Throwable `Either` A] =
    TaskEffect.taskAttempt(e)

  def runTaskMemo[U](cache: Cache)(using m: Member.Aux[org.atnos.eff.Memoized, R, U], task: Task |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskMemo(key: AnyRef, cache: Cache)(using task: Task /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)