package par

import domain.Key
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.std.Dispatcher
import domain.*

import scala.scalanative.posix.signal
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.scalanative.posix.unistd

object SignalHandler:
  var dispatcher: Dispatcher[IO] = null
  var eventQueue: Queue[IO, Event] = null

  val sigwinchHandler: CFuncPtr1[CInt, Unit] = (_: CInt) =>
    if dispatcher != null && eventQueue != null then 
      dispatcher.unsafeRunAndForget(
        eventQueue.offer(Event.WindowResize).void
      )
    ()

object EventLoop:
  private final val SIGWINCH = 28 // Not in scala-native posix

  def create(
      queue: Queue[IO, Event],
      dispatcher: Dispatcher[IO]
  ): IO[Unit] =
    for
      _ <- IO {
        SignalHandler.eventQueue = queue
        SignalHandler.dispatcher = dispatcher
        signal.signal(SIGWINCH, SignalHandler.sigwinchHandler)
      }
      _ <- keyReaderFiber(queue).start.void
    yield ()

  private def keyReaderFiber(queue: Queue[IO, Event]): IO[Unit] =
    (for
      key <- IO.blocking(readKey())
      _ <- queue.offer(Event.Key(key))
    yield ()).foreverM

  private def readKey(): Key =
    def readUntil: Byte =
      val buf = stackalloc[Byte]()
      val nread = unistd.read(unistd.STDIN_FILENO, buf, 1.toUInt)
      if nread == -1 then throw new Exception("read")
      !buf

    def readFollowingKey: Option[Byte] =
      val a = stackalloc[CChar]()
      if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then None
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
    if a == escInt.toByte then
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
    end if
  end readKey
end EventLoop
