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
import scala.scalanative.unsigned.*
import scala.util.Try
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

inline val KILO_VERSION = "0.0.1"

case class Row(chars: ArrayBuffer[Byte])

case class EditorConfig(cx: Int, cy: Int, screenRows: Int, screenCols: Int, rows: ArrayBuffer[Row] = ArrayBuffer.empty)

type EditorConfigState[F[_], A] = StateT[F, EditorConfig, A]

type EditorBufState[F[_]] = StateT[F, String, Unit]

type EitherRawResult[A] = Either[Int, A]

enum PageKey:
  case Up
  case Down

enum AKey:
  case Left
  case Right
  case Up
  case Down

enum Key:
  case Arrow(k: AKey)
  case CtrlArrow(k: AKey)
  case Page(k: PageKey)
  case Home
  case End
  case Delete
  case Escape
  case Char(c: CChar)

import Key.*
import PageKey.*

val wd = os.pwd

object Main extends IOApp:
  inline val escInt = 0x1b
  inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
  val EXIT = ctrlKey('q')
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

  def editorOpen[F[_]: MonadThrow](filename: String): EditorConfigState[F, Unit] =
    val s = os.read.lines.stream(wd / filename)
    s.head
    val line = "Hello world".toCharArray().map(_.toByte)
    for
      headOpt <- StateT.liftF(os.read.lines.stream(wd / filename).headOption.pure)
      _ <- StateT.modify[F, EditorConfig](c =>
        headOpt.fold(c)(line => c.copy(rows = c.rows.addOne(Row(ArrayBuffer.from(line.toSeq.map(_.toByte))))))
      )
    yield ()

  def editorReadKey[F[_]: MonadThrow: Defer](): F[Key] =
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
      read.run((0, ref)).flatMap(_._1._2.get).map(a => !a)
    }
    for
      c <- fc
      r <-
        // c.pure
        if c == escInt.toByte then
          Defer[F].defer {
            {
              val a = stackalloc[CChar]()
              val b = stackalloc[CChar]()
              val c = stackalloc[CChar]()
              if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then Escape
              else if unistd.read(unistd.STDIN_FILENO, b, 1.toUInt) != 1 then Escape
              else if !a == '[' then
                (!b) match
                  case 'A' => Arrow(AKey.Up)
                  case 'B' => Arrow(AKey.Down)
                  case 'C' => Arrow(AKey.Right)
                  case 'D' => Arrow(AKey.Left)
                  case 'H' => Home
                  case 'F' => End
                  case bv if bv >= '0' && bv <= '9' =>
                    if unistd.read(unistd.STDIN_FILENO, c, 1.toUInt) != 1 then Escape
                    else if !c == '~' then
                      bv match
                        case '1' | '7' => Home
                        case '4' | '8' => End
                        case '3'       => Delete
                        case '5'       => Page(Up)
                        case '6'       => Page(Down)
                    else Escape
                  case _ => Escape
              else if !a == 'O' then
                (!b) match
                  case 'H' => Home
                  case 'F' => End
                  case _   => Escape
              else Escape
              end if
            }.pure
          }
        else Char(c).pure
    yield r
    end for
  end editorReadKey

  def editorMoveCursor[F[_]: MonadThrow](key: AKey): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { e =>
      key match
        case AKey.Left  => if e.cx != 0 then e.copy(cx = e.cx - 1) else e
        case AKey.Right => if e.cx != e.screenCols - 1 then e.copy(cx = e.cx + 1) else e
        case AKey.Up    => if e.cy != 0 then e.copy(cy = e.cy - 1) else e
        case AKey.Down  => if e.cy != e.screenRows - 1 then e.copy(cy = e.cy + 1) else e
    }

  def editorProcessKeypress[F[_]: MonadThrow: Defer](): EditorConfigState[F, EitherRawResult[Unit]] =
    for
      k <- StateT.liftF(editorReadKey())
      config <- StateT.get
      r <- k match
        case Char(EXIT) => StateT.liftF(resetScreenCursorTask >> Left(0).pure)
        case Home       => StateT.modify[F, EditorConfig](_.copy(cx = 0)) >> Right(()).pure
        case End        => StateT.modify[F, EditorConfig](e => e.copy(cx = e.screenCols - 1)) >> Right(()).pure
        case Arrow(a) =>
          editorMoveCursor(a) >> StateT.liftF(Right(()).pure)
        case Page(a) =>
          editorMoveCursor(if a == Up then AKey.Up else AKey.Down).replicateA_(config.screenRows) >> StateT
            .liftF(Right(()).pure)
        case _ => StateT.liftF(Right(()).pure)
    yield r

  def editorDrawRows[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
    StateT.modify((s: String) =>
      s ++ (0 until config.screenRows)
        .foldLeft(StringBuilder.newBuilder)((bldr, idx) =>
          if idx >= config.rows.size then
            if idx == (config.screenRows / 3) then
              val welcomeDisplayLen = if welcome.size > config.screenCols then config.screenCols else welcome.size
              val padding = (config.screenCols - welcomeDisplayLen) / 2
              if padding > 0 then bldr ++= "~"
              if padding - 1 > 0 then bldr ++= " ".repeat(padding - 1)
              bldr ++= welcome.substring(0, welcomeDisplayLen)
            else if idx < config.screenRows - 1 then bldr ++= s"~${eraseInLine.esc}\r\n"
            else bldr ++= s"~${eraseInLine.esc}"
          else
            val len = config.rows(0).chars.size
            bldr ++= config
              .rows(0)
              .chars
              .slice(0, if len > config.screenCols then config.screenCols else len)
              .map(_.toChar)
              .mkString
        )
        .toString()
    )

  def editorRefreshScreen[F[_]: MonadThrow](config: EditorConfig): F[Unit] =
    val a = for
      _ <- StateT.set("")
      // _ <- StateT.modify[F, String](_ ++ "12345;laksdjc;lkasjdc;klasjd;clk")
      _ <- StateT.modify[F, String](_ ++ escJoinStr(hideCursor, resetCursor))
      // config <- get[R, EditorConfig]()
      _ <- editorDrawRows(config)
      _ <- StateT.modify[F, String](_ ++ escJoinStrR(setCursoer(config.cx, config.cy), showCursor))
      s <- StateT.get[F, String]
      _ <- StateT.liftF(Zone {
        unistd.write(unistd.STDOUT_FILENO, toCString(s), s.size.toUInt)
      }.pure)
    yield ()
    a.run("").map(_._2)

    // Defer[F].defer(println("test").pure)
  end editorRefreshScreen

  def program[F[_]: MonadThrow: Defer](filename: String): EditorConfigState[F, Unit] =
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
      _ <- editorOpen(filename)
      _ <- go
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        args.headOption.fold(Task.unit)(filename => program[Task](filename).run(EditorConfig(0, 0, 0, 0)).map(_._2))
      )
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
