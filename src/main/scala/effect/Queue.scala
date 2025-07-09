package effect

import cats.syntax.all.*
import scala.collection.immutable.{Queue as ScalaQueue}
import scala.concurrent.duration.DurationInt

trait Queue[A]:
  def offer(a: A): Task[Unit]
  def take: Task[A]

object Queue:
  def unbounded[A]: Task[Queue[A]] =
    type State = ScalaQueue[A]
    Ref.of[State](ScalaQueue.empty).map { state =>
      new Queue[A]:
        def offer(a: A): Task[Unit] =
          state.update(_.enqueue(a))

        def take: Task[A] =
          state.modify {
            case q if q.nonEmpty =>
              val (a, rest) = q.dequeue
              (rest, Task.now(a))
            case _ =>
              (ScalaQueue.empty, Task.sleep(1.millisecond).flatMap(_ => take))
          }.flatten
    }
