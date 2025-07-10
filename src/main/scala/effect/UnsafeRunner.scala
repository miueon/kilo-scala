package effect

import java.util.concurrent.ExecutorService
import scala.util.control.NonFatal

/** A minimal unsafe runner for Task effects, designed for use in signal handlers and other impure contexts where we
  * cannot properly manage Task execution.
  */
object UnsafeRunner:
  private val defaultPool = java.util.concurrent.ForkJoinPool.commonPool()

  /** Runs a Task effect unsafely, attempting to execute it as immediately as possible. This is similar to
    * Dispatcher.unsafeRunAndForget in Cats Effect.
    *
    * WARNING: This should only be used in contexts where proper Task execution is impossible (e.g., signal handlers,
    * native callbacks).
    *
    * Uses a direct approach to try to run the effect as soon as possible since signal handlers need immediate
    * execution.
    */
  def unsafeRunAndForget[A](task: Task[A])(using pool: ExecutorService = defaultPool): Unit =
    try
      // Try to run immediately in the current thread if possible
      // This mimics the behavior needed for signal handlers
      val runnable = new Runnable:
        def run(): Unit =
          try task.unsafeRunSync(pool)
          catch case NonFatal(_) => () // Ignore errors in fire-and-forget

      // Submit to the pool but don't wait for completion
      pool.execute(runnable)
    catch case NonFatal(_) => () // If even submitting fails, ignore
end UnsafeRunner
