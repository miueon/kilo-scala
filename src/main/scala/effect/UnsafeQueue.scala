package effect

import java.util.concurrent.ConcurrentLinkedQueue
import cats.syntax.all.*
import scala.concurrent.duration.DurationInt
import cats.syntax.flatMap.*

trait UnsafeQueue[A]:
  def offer(a: A): Unit
  def take: Task[A]

object UnsafeQueue:
  def unbounded[A]: Task[UnsafeQueue[A]] =
    Task {
      val queue = new ConcurrentLinkedQueue[A]()
      new UnsafeQueue[A]:
        def offer(a: A): Unit = queue.add(a)
        def take: Task[A] =
          Task(queue.poll()).flatMap {
            case null => Task.sleep(1.millisecond).flatMap(_ => take)
            case a    => Task.now(a)
          }
    }
