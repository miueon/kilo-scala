import `macro`.*
import cats.Defer
import cats.Eval
import cats.Monad
import cats.MonadThrow
import cats.data.EitherT
import cats.data.Kleisli
import cats.data.Reader
import cats.data.State
import cats.data.StateT
import cats.data.Writer
import cats.syntax.all.*
import effect.*
import effect.*
import effect.TaskEffect.*
import effect.TaskInterpretation.*
import effect.pull.Stream
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*
import rawmode.*
import rawmode.all.*
import rawmode.all.disableRawMode as resetRawMode
import rawmode.all.enableRawMode as setRawMode
import util.Utils.*

import java.nio.charset.Charset
import java.util.concurrent.Executors
import scala.scalanative.libc.{errno as libcErrno, *}
import scala.scalanative.posix.cpio
import scala.scalanative.posix.errno
import scala.scalanative.posix.sys.ioctl
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
import scala.util.Try
import scala.annotation.tailrec

inline val KILO_VERSION = "0.0.1"

case class EditorConfig(cx: Int, cy: Int, screenRows: Int, screenCols: Int)

type _editorConfigState[R] = State[EditorConfig, *] |= R

type EitherRawResult[A] = Either[Int, A]

type _editorBuf[R] = State[String, *] |= R

object Main extends IOApp:
  inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
  inline val hideCursor = "[?25l"
  inline val showCursor = "[?25h"
  inline val clearScreen = "[2J"
  inline val eraseInLine = "[K"
  inline val resetCursor = "[H"
  inline val welcome = "Kilo editor -- version " + KILO_VERSION
  inline def resetScreenCursorStr = escJoinStr(clearScreen, resetCursor)
  def setCursoer(x: Int, y: Int) = s"[${y + 1};${x + 1}H"
  def escJoinStrR(xs: String*): String = xs.toSeq.mkString(esc, esc, "")

  inline def resetScreenCursorTask = Task(Zone {
    unistd.write(unistd.STDOUT_FILENO, toCString(resetScreenCursorStr), resetScreenCursorStr.size.toUInt)
  }).void

  def getWindowSize[F[_]: MonadThrow: Defer]: F[EitherRawResult[(Int, Int)]] =
    Defer[F].defer {
      {
        val ws = stackalloc[winsize]()
        if ioctl.ioctl(unistd.STDOUT_FILENO, get_TIOCGWINSZ(), ws.asInstanceOf[Ptr[Byte]]) == -1 || (!ws).ws_col == 0
        then Left(-1)
        else Right((!ws).ws_col.toInt, (!ws).ws_row.toInt)
      }.pure
    }

  def initEditor[R: _editorConfigState: _task]: Eff[R, Unit] =
    for
      result <- fromTask(getWindowSize)
      _ <- result match
        case Left(-1) => fromTask(Task.raiseError(new Exception("getWindowSize")))
        case Right((col, row)) =>
          modify((s: EditorConfig) => s.copy(screenRows = row, screenCols = col))
        case _ => Eff.pure(())
    yield ()

  def editorReadKey[F[_]: MonadThrow: Defer](ref: Ref[F, Ptr[CChar]]): F[Unit] =
    for
      cPtr <- ref.get
      nread <- Defer[F].defer(unistd.read(unistd.STDIN_FILENO, cPtr, UInt.valueOf(1)).pure)
      _ <-
        if nread != 1 then editorReadKey(ref)
        else if nread == -1 && errno.errno != errno.EAGAIN then new Exception("read").raiseError
        else MonadThrow[F].unit
    yield ()

  def editorMoveCursor[R: _editorConfigState](key: CChar): Eff[R, Unit] =
    modify[R, EditorConfig] { e =>
      key match
        case 'a' => e.copy(cx = e.cx - 1)
        case 'd' => e.copy(cx = e.cx + 1)
        case 'w' => e.copy(cy = e.cy - 1)
        case 's' => e.copy(cy = e.cy + 1)
    }

  def editorProcessKeypress[R: _task: _editorBuf: _editorConfigState](): Eff[R, EitherRawResult[Unit]] =
    for
      cPtrRef <- fromTask(Ref[Task, Ptr[CChar]](malloc[CChar]).pure)
      _ <- fromTask(editorReadKey(cPtrRef))
      cPtr <- fromTask(cPtrRef.get)
      r <- !cPtr match
        case a if a == ctrlKey('q')      => fromTask(resetScreenCursorTask >> Task(Left(0)))
        case a @ ('w' | 's' | 'a' | 'd') => editorMoveCursor(a) >> Right(()).pure
        case _                           => Eff.pure(Right(()))
      _ <- fromTask(Task(stdlib.free(cPtr)))
    yield r

  def editorDrawRows[R: _editorBuf](screenRows: Int, screenCols: Int): Eff[R, Unit] =
    modify((s: String) =>
      s ++ (1 to screenRows)
        .foldLeft(StringBuilder.newBuilder)((bldr, idx) =>
          if idx == (screenRows / 3) - 1 then
            val welcomeDisplayLen = if welcome.size > screenCols then screenCols else welcome.size
            val padding = (screenCols - welcomeDisplayLen) / 2
            if padding > 0 then bldr ++= "~"
            if padding - 1 > 0 then bldr ++= " ".repeat(padding - 1)
            bldr ++= welcome.substring(0, welcomeDisplayLen)
          else if idx < screenRows then bldr ++= s"~${eraseInLine.esc}\r\n"
          else bldr ++= s"~${eraseInLine.esc}"
        )
        .toString()
    )
  import org.atnos.eff.Members.extractMember
  def editorRefreshScreen[R: _editorConfigState: _task: _editorBuf](): Eff[R, Unit] =
    for
      _ <- modify[R, String](_ ++ escJoinStr(hideCursor, resetCursor))
      config <- get[R, EditorConfig]()
      _ <- editorDrawRows(config.screenRows, config.screenCols)
      _ <- modify[R, String](_ ++ escJoinStrR(setCursoer(config.cx, config.cy), showCursor))
      s <- get[R, String]
      _ <- fromTask(Task(Zone {
        unistd.write(unistd.STDOUT_FILENO, toCString(s), s.size.toUInt)
      }))
      _ <- modify[R, String](_ => "")
    yield ()
  end editorRefreshScreen

  type AppStack = Fx.fx3[State[String, *], State[EditorConfig, *], Task]

  def program: Eff[AppStack, Unit] =
    def test: Eff[AppStack, Unit] =
      for
        _ <- fromTask[AppStack, Unit](Task(Thread.sleep(1)))
        _ <- fromTask[AppStack, Unit](Task(println("test")))
      yield ()
    def go(): Eff[AppStack, Unit] =
      for
        _ <- editorRefreshScreen[AppStack]()
        r <- editorProcessKeypress[AppStack]()
        _ <- if r.isLeft then Eff.pure(()) else go()
      yield ()
    for
      _ <- initEditor[AppStack]
      _ <- go()
    yield ()

  def pureMain(args: List[String]): IO[Unit] =
    // program.runState(EditorConfig(0, 0, 0, 0)).runState("").toTask.void.asIO
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ => program.runState(EditorConfig(0, 0, 0, 0)).runState("").toTask.void)
      .handleErrorWith(e =>
        resetScreenCursorTask >>
          Task.apply(
            Zone {
              stdio.printf(c"%s\n%s\n", toCString(e.getMessage), string.strerror(errno.errno))
            }
          )
      )
      .asIO
      .void
  end pureMain
end Main
