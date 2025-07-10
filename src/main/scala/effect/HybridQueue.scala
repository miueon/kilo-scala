package effect

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import cats.syntax.all.*

/** A hybrid queue that provides immediate offer semantics for signal handlers while maintaining safe Task-based take
  * operations for the main event loop.
  *
  * This version uses a semaphore to avoid polling altogether, providing efficient blocking behavior.
  */
trait HybridQueue[A]:
  /** Safe offer that returns Task[Unit] for normal application use */
  def offer(a: A): Task[Unit]

  /** Unsafe immediate offer for signal handlers - returns Unit immediately */
  def unsafeOffer(a: A): Unit

  /** Safe take operation that blocks efficiently using semaphore */
  def take: Task[A]

object HybridQueue:
  def unbounded[A]: Task[HybridQueue[A]] =
    Task {
      // Use ConcurrentLinkedQueue for immediate access from signal handlers
      val queue = new ConcurrentLinkedQueue[A]()
      // Use semaphore for efficient blocking without polling
      val semaphore = new Semaphore(0)

      new HybridQueue[A]:
        def offer(a: A): Task[Unit] =
          Task {
            queue.add(a)
            semaphore.release()
          }

        def unsafeOffer(a: A): Unit =
          queue.add(a)
          semaphore.release()

        def take: Task[A] =
          Task {
            semaphore.acquire() // Block until an item is available
            queue.poll() match
              case null =>
                // This should never happen, but if it does, try again
                semaphore.release() // Release the permit we just took
                throw new RuntimeException("Queue was empty after semaphore acquire")
              case a => a
          }
      end new
    }
end HybridQueue
