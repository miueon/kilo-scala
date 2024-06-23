package effect

import java.util.concurrent.atomic.AtomicReference
import java.lang.RuntimeException
import cats.Monad
import cats.syntax.all.*
import cats.MonadThrow
import cats.instances.finiteDuration

final class Ref[F[_], A] private (
    underlying: AtomicReference[A],
    delay: [A] => (() => A) => F[A]
):
  def get: F[A] = delay(() => underlying.get)
  def set(a: A): F[Unit] = delay(() => underlying.set(a))
  def modify[B](f: A => (A, B)): F[B] = delay { () =>
    def loop(): B =
      val oldA = underlying.get
      val (newA, result) = f(oldA)
      if underlying.compareAndSet(oldA, newA) then result else loop()
    loop()
  }

object Ref:
  def apply[F[_]: Monad, A](init: A): Ref[F, A] =
    new Ref(
      AtomicReference[A](init),
      [A] => (th: () => A) => ().pure.map(_ => th())
    )

final class Scope[F[_]: MonadThrow](
    parent: Option[Scope[F]],
    val id: Id,
    state: Ref[F, Scope.State[F]]
):
  import Scope.State

  def open(finalizer: F[Unit]): F[Scope[F]] =
    state.modify {
      case State.Open(myFinalizer, subscopes) =>
        val sub = new Scope(Some(this), new Id, Ref(State.Open(finalizer, Vector.empty)))
        State.Open(myFinalizer, subscopes :+ sub) -> sub.pure
      case State.Closed() =>
        val next = parent match
          case None    => RuntimeException("root scope already closed").raiseError
          case Some(p) => p.open(finalizer)
        State.Closed() -> next

    }.flatten

  def close: F[Unit] =
    state.modify {
      case State.Open(finalizer, subscopes) =>
        val finalizers = (subscopes.reverseIterator.map(_.close) ++ Iterator(finalizer)).toList
        def go(rem: List[F[Unit]], error: Option[Throwable]): F[Unit] =
          rem match
            case Nil =>
              error match
                case None    => ().pure
                case Some(t) => t.raiseError
            case hd :: tl =>
              for
                res <- hd.attempt
                _ <- go(tl, error orElse res.swap.toOption)
              yield ()
        State.Closed() -> go(finalizers, None)
      case State.Closed() => State.Closed() -> ().pure
    }.flatten

  def findScope(target: Id): F[Option[Scope[F]]] =
    findThisOrSubScope(target).flatMap {
      case Some(s) => s.some.pure
      case None =>
        parent match
          case Some(p) => p.findScope(target)
          case None    => None.pure
    }

  def findThisOrSubScope(target: Id): F[Option[Scope[F]]] =
    if id == target then this.some.pure
    else
      state.get.flatMap {
        case State.Open(_, subscopes) =>
          def go(rem: List[Scope[F]]): F[Option[Scope[F]]] =
            rem match
              case Nil => None.pure
              case hd :: tl =>
                hd.findThisOrSubScope(target).flatMap {
                  case Some(s) => s.some.pure
                  case None    => go(tl)
                }
          go(subscopes.toList)
        case State.Closed() => None.pure
      }
end Scope

object Scope:
  enum State[F[_]]:
    case Open(finalizer: F[Unit], subscope: Vector[Scope[F]]) extends State[F]
    case Closed() extends State[F]

  def root[F[_]: MonadThrow]: Scope[F] =
    new Scope(None, new Id, Ref(State.Open(().pure, Vector.empty)))
