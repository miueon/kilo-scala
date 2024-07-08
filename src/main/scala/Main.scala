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
import effect.pull.Stream
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

type EditorConfigState[F[_], A] = StateT[F, EditorConfig, A]

type EditorBufState[F[_]] = StateT[F, String, Unit]

type EitherRawResult[A] = Either[Int, A]

object Main extends IOApp:
  inline def escByte = 0x1b.toByte
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

  inline def resetScreenCursorTask[F[_]: MonadThrow] = Zone {
    unistd.write(unistd.STDOUT_FILENO, toCString(resetScreenCursorStr), resetScreenCursorStr.size.toUInt)
  }.pure.void

  def getWindowSize[F[_]: MonadThrow: Defer]: F[EitherRawResult[(Int, Int)]] =
    Defer[F].defer {
      {
        val ws = stackalloc[winsize]()
        if ioctl.ioctl(
            unistd.STDOUT_FILENO,
            get_TIOCGWINSZ(),
            ws.asInstanceOf[Ptr[Byte]]
          ) == -1 || (!ws).ws_col.toInt == 0
        then Left(-1)
        else Right((!ws).ws_col.toInt, (!ws).ws_row.toInt)
      }.pure
    }

  def initEditor[F[_]: MonadThrow: Defer]: EditorConfigState[F, Unit] =
    for
      result <- StateT.liftF(getWindowSize)
      _ <- result match
        case Left(_) => StateT.liftF(MonadThrow[F].raiseError(new Exception("getWindowSize")))
        case Right((col, row)) =>
          StateT.modify[F, EditorConfig](_.copy(screenRows = row, screenCols = col))
        // case _ => StateT.liftF(().pure)
    yield ()

  def editorReadKey[F[_]: MonadThrow: Defer](): F[Byte] =
    val read = Monad[StateT[F, (Int, Ref[F, Ptr[CChar]]), *]]
      .whileM_(
        for
          (_, cPtrRef) <- StateT.get
          cPtr <- StateT.liftF(cPtrRef.get)
          nread <- StateT.liftF(unistd.read(unistd.STDIN_FILENO, cPtr, 1.toUInt).pure)
          _ <- StateT.set((nread, cPtrRef))
        yield nread != 1
      )(
        for
          (nread, _) <- StateT.get()
          _ <- StateT.liftF {
            if nread == -1 then new Exception("read").raiseError else MonadThrow[F].unit
          }
        yield ()
      )
    val fc = Zone {
      val ref = Ref[F, Ptr[CChar]](alloc())
      // val p = for
      //   (_, cPtrRef) <- read.get
      //   c <- StateT.liftF(cPtrRef.get.map(!_))
      //   r <-
      //     StateT.liftF(Defer[F].defer {
      //       if c == esc.toByte then
      //         // {
      //         //   val a = alloc[CChar]()
      //         //   val b = alloc[CChar]()
      //         //   if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then esc.toByte
      //         //   else if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then esc.toByte
      //         //   else if !a == '[' then
      //         //     (!b) match
      //         //       case 'A' => 'w'.toByte
      //         //       case 'B' => 's'.toByte
      //         //       case 'C' => 'd'.toByte
      //         //       case 'D' => 'a'.toByte
      //         //       case _   => esc.toByte
      //         //   else esc.toByte
      //         // }.pure
      //         c.pure
      //       else c.pure
      //     })
      // yield r
      read.run((0, ref)).flatMap(_._1._2.get).map(a => !a)
    }
    for
      c <- fc
      _ <- 
        println(s"c${c} and ${c == escByte}").pure 
      r <- 
        // c.pure
        if c == escByte then
          Defer[F].defer {{
            val a = stackalloc[CChar]()
            val b = stackalloc[CChar]()
            if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then escByte
            else if unistd.read(unistd.STDIN_FILENO, b, 1.toUInt) != 1 then escByte
            else if !a == '[' then
              println(s"${!a} ${!b}")
              (!b) match
                case 'A' => 'w'.toByte
                case 'B' => 's'.toByte
                case 'C' => 'd'.toByte
                case 'D' => 'a'.toByte
                case _   => escByte
            else escByte
          }.pure
        }
        else c.pure
      _ <- println(s"result ${r}").pure
    yield r
    end for
  end editorReadKey

  def editorMoveCursor[F[_]: MonadThrow](key: CChar): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { e =>
      key match
        case 'a' => e.copy(cx = e.cx - 1)
        case 'd' => e.copy(cx = e.cx + 1)
        case 'w' => e.copy(cy = e.cy - 1)
        case 's' => e.copy(cy = e.cy + 1)
    }

  def editorProcessKeypress[F[_]: MonadThrow: Defer](): EditorConfigState[F, EitherRawResult[Unit]] =
    for
      // cPtrRef <- StateT.liftF(
      //   MonadThrow[F].pure(()) >>
      //     Ref[F, Ptr[CChar]](malloc[CChar]).pure
      // )
      k <- StateT.liftF(editorReadKey())
      // cPtr <- StateT.liftF(cPtrRef.get)
      // k = !cPtr
      // _ <- StateT.liftF(MonadThrow[F].pure(()) >> stdlib.free(cPtr).pure)
      r <- k match
        case a if a == ctrlKey('q')      => StateT.liftF(resetScreenCursorTask >> Left(0).pure)
        case a @ ('w' | 's' | 'a' | 'd') => editorMoveCursor(a) >> StateT.liftF(Right(()).pure)
        case _                           => StateT.liftF(Right(()).pure)
    yield r

  def editorDrawRows[F[_]: MonadThrow](screenRows: Int, screenCols: Int): EditorBufState[F] =
    StateT.modify((s: String) =>
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

  def editorRefreshScreen[F[_]: MonadThrow](config: EditorConfig): F[Unit] =
    val a = for
      _ <- StateT.set("")
      // _ <- StateT.modify[F, String](_ ++ "12345;laksdjc;lkasjdc;klasjd;clk")
      _ <- StateT.modify[F, String](_ ++ escJoinStr(hideCursor, resetCursor))
      // config <- get[R, EditorConfig]()
      _ <- editorDrawRows(config.screenRows, config.screenCols)
      _ <- StateT.modify[F, String](_ ++ escJoinStrR(setCursoer(config.cx, config.cy), showCursor))
      s <- StateT.get[F, String]
      _ <- StateT.liftF(Zone {
        unistd.write(unistd.STDOUT_FILENO, toCString(s), s.size.toUInt)
      }.pure)
    yield ()
    a.run("").map(_._2)

    // Defer[F].defer(println("test").pure)
  end editorRefreshScreen

  def program[F[_]: MonadThrow: Defer]: EditorConfigState[F, Unit] =
    def go: EditorConfigState[F, Unit] =
      val a = for
        config <- StateT.get[F, EditorConfig]
        _ <- StateT.liftF(editorRefreshScreen(config))
      yield ()
      a >>
        Monad[StateT[F, EditorConfig, *]].whileM_(
          editorProcessKeypress().map(_.isRight)
        )(
          a
        )
    for
      _ <- initEditor
      _ <- go
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ => program[Task].run(EditorConfig(0, 0, 0, 0)).map(_._2))
      .handleErrorWith(e =>
        resetScreenCursorTask[Task] >>
          Task.apply(
            Zone {
              stdio.printf(c"%s\n%s\n", toCString(e.getMessage))
            }
          )
      )
      .asIO
      .void

end Main
