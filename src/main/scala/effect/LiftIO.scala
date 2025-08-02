package effect

import cats.effect.IO
import cats.data.StateT

trait LiftIO[F[_]]:
  def lift[A](fa: IO[A]): F[A]

object LiftIO:
  def apply[F[_]](using ev: LiftIO[F]): LiftIO[F] = ev

  given liftIOForIO: LiftIO[IO] with
    def lift[A](fa: IO[A]): IO[A] = fa

  given liftIOForStateT[S]: LiftIO[StateT[IO, S, *]] with
    def lift[A](fa: IO[A]): StateT[IO, S, A] = StateT.liftF(fa)

  extension [F[_]](liftIO: LiftIO[F])
    def lift[A](fa: IO[A]): F[A] = liftIO.lift(fa)