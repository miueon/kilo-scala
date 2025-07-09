package par

import domain.Key
import effect.Task
import rawmode.all.*
import domain.*
import par.Event

import scala.scalanative.posix.signal
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.posix.unistd
import cats.syntax.all.*
import java.util.concurrent.ConcurrentLinkedQueue

object SignalHandler:
  var eventQueue: ConcurrentLinkedQueue[Event] = null

  val sigwinchHandler: CFuncPtr1[CInt, Unit] = (signo: CInt) => {
    if (eventQueue != null) {
      eventQueue.add(Event.WindowResize)
    }
    ()
  }

object EventLoop:
  private final val SIGWINCH = 28 // Not in scala-native posix

  def create(queue: ConcurrentLinkedQueue[Event]): Task[Unit] =
    Task {
      SignalHandler.eventQueue = queue
      signal.signal(SIGWINCH, SignalHandler.sigwinchHandler)

      val keyReader: Runnable = () => {
        while (true) {
          val key = readKey()
          if (queue != null) {
            queue.add(Event.Key(key))
          }
        }
      }
      val thread = new Thread(keyReader)
      thread.start()
    }

  private def readKey(): Key =
    def readUntil: Byte =
      val buf = stackalloc[Byte]()
      val nread = unistd.read(unistd.STDIN_FILENO, buf, 1.toUInt)
      if (nread == -1) throw new Exception("read")
      !buf

    def readFollowingKey: Option[Byte] =
      val a = stackalloc[CChar]()
      if (unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1) None
      else Some(!a)

    def readArrow(c: Option[Byte], d: Option[Byte]): Key =
      import domain.AKey
      import domain.PageKey
      (c, d) match
        case (Some('1' | '7'), Some('~')) => Key.Home
        case (Some('4' | '8'), Some('~')) => Key.End
        case (Some('3'), Some('~'))       => Key.Delete
        case (Some('5'), Some('~'))       => Key.Page(PageKey.Up)
        case (Some('6'), Some('~'))       => Key.Page(PageKey.Down)
        case (Some('5'), Some('A'))       => Key.CtrlArrow(AKey.Up)
        case (Some('5'), Some('B'))       => Key.CtrlArrow(AKey.Down)
        case (Some('5'), Some('C'))       => Key.CtrlArrow(AKey.Right)
        case (Some('5'), Some('D'))       => Key.CtrlArrow(AKey.Left)
        case _                            => Key.Escape

    val a = readUntil
    if (a == escInt.toByte)
      readFollowingKey match
        case Some('[') =>
          import domain.AKey
          readFollowingKey match
            case Some('A') => Key.Arrow(AKey.Up)
            case Some('B') => Key.Arrow(AKey.Down)
            case Some('C') => Key.Arrow(AKey.Right)
            case Some('D') => Key.Arrow(AKey.Left)
            case Some('H') => Key.Home
            case Some('F') => Key.End
            case c @ Some(cv) if cv >= '0' && cv <= '9' =>
              val d = readFollowingKey
              (c, d) match
                case (Some('1'), Some(';')) => readArrow(readFollowingKey, readFollowingKey)
                case _                      => readArrow(c, d)
            case _ => Key.Escape
        case Some('0') =>
          import domain.AKey
          readFollowingKey match
            case Some('a') => Key.CtrlArrow(AKey.Up)
            case Some('b') => Key.CtrlArrow(AKey.Down)
            case Some('c') => Key.CtrlArrow(AKey.Right)
            case Some('d') => Key.CtrlArrow(AKey.Left)
            case _         => Key.Escape
        case _ => Key.Escape
    else Key.Char(a)