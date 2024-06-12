package par

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Callable

final class Actor[A](es: ExecutorService)(
    handler: A => Unit,
    onError: Throwable => Unit = throw _
):
  self =>

  private val tail = new AtomicReference(new Node[A]())
  private val suspended = AtomicInteger(1)
  private val head = AtomicReference(tail.get)

  def !(a: A): Unit =
    val n = Node(a)
    head.getAndSet(n).lazySet(n)
    trySchedule()

  def contramap[B](f: B => A): Actor[B] =
    Actor[B](es)(b => this ! f(b), onError)

  private def trySchedule(): Unit =
    if suspended.compareAndSet(1, 0) then schedule()

  private def schedule(): Unit =
    es.submit(
      new Callable[Unit]:
        def call(): Unit = act()
    )

  private def act(): Unit =
    val t = tail.get
    val n = batchHandle(t, 1024)
    if n ne t then
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    else
      suspended.set(1)
      if n.get ne null then trySchedule()

  @annotation.tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] =
    val n = t.get
    if n ne null then
      try handler(n.a)
      catch case ex: Throwable => onError(ex)

      if i > 0 then batchHandle(n, i - 1) else n
    else t

private class Node[A](var a: A = null.asInstanceOf[A])
    extends AtomicReference[Node[A]]
