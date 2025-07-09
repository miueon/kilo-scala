package effect

import java.util.concurrent.atomic.AtomicReference
import cats.syntax.all.toFunctorOps

trait Ref[A]:
  def get: Task[A]
  def set(a: A): Task[Unit]
  def update(f: A => A): Task[Unit]
  def modify[B](f: A => (A, B)): Task[B]

object Ref:
  def of[A](a: A): Task[Ref[A]] = Task {
    new Ref[A]:
      private val ref = new AtomicReference(a)

      def get: Task[A] = Task(ref.get())

      def set(a: A): Task[Unit] = Task(ref.set(a))

      def update(f: A => A): Task[Unit] =
        modify(a => (f(a), ())).void

      def modify[B](f: A => (A, B)): Task[B] = Task.delay {
        @annotation.tailrec
        def go(): B = {
          val current = ref.get()
          val (updated, result) = f(current)
          if (ref.compareAndSet(current, updated)) result
          else go()
        }
        go()
      }
  }
