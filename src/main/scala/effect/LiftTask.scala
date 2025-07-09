package effect

import cats.data.StateT

trait LiftTask[F[_]]:
  def lift[A](task: Task[A]): F[A]

object LiftTask:
  def apply[F[_]](using lt: LiftTask[F]): LiftTask[F] = lt

  given [S]: LiftTask[StateT[Task, S, *]] with
    def lift[A](task: Task[A]): StateT[Task, S, A] = StateT.liftF(task)
