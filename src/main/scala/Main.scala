import `macro`.*
import cats.Defer
import cats.Eval
import cats.Monad
import cats.MonadThrow
import cats.data.EitherT
import cats.data.Kleisli
import cats.data.OptionT
import cats.data.Reader
import cats.data.State
import cats.data.StateT
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
import java.time.Instant
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
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

inline val KILO_VERSION = "0.0.1"
inline val KILO_TAB_STOP = 2
inline val KILO_MSG = "HELP: Ctrl-Q = quit"

case class Row(chars: ArrayBuffer[Byte], render: String)

case class StatusMessage(
    msg: String,
    time: Instant = Instant.now()
)

case class EditorConfig(
    cx: Int,
    cy: Int,
    rx: Int,
    rowoff: Int,
    coloff: Int,
    screenRows: Int,
    screenCols: Int,
    statusMsg: Option[StatusMessage] = None,
    rows: ArrayBuffer[Row] = ArrayBuffer.empty,
    filename: Option[String] = None
)

type EditorConfigState[F[_], A] = StateT[F, EditorConfig, A]

type EditorBufState[F[_]] = StateT[F, StringBuilder, Unit]

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

extension (s: String) def removeSuffixNewLine: String = s.stripSuffix("\n").stripSuffix("\r")

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
  def escJoinStrR(xs: String*): String = xs.toSeq.mkString(esc, esc, "")

  inline def resetScreenCursorTask[F[_]: MonadThrow] =
    print(resetScreenCursorStr).pure

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
          StateT.modify[F, EditorConfig](_.copy(screenRows = row - 2, screenCols = col))
    yield ()

  def editorOpen[F[_]: MonadThrow](filenameOpt: Option[String]): EditorConfigState[F, Unit] =
    filenameOpt.fold[EditorConfigState[F, Unit]]((()).pure)(filename =>
      for
        rows <- StateT.liftF(os.read.lines(wd / filename).pure)
        _ <- StateT.modify[F, EditorConfig](c =>
          c.copy(
            rows = rows
              .map(r =>
                val arr = ArrayBuffer(r.removeSuffixNewLine.getBytes*)
                Row(arr, editorUpdateRow(arr))
              )
              .to(ArrayBuffer),
            filename = filename.some
          )
        )
      yield ()
    )

  def editorUpdateRow(arr: ArrayBuffer[Byte]): String =
    arr.flatMap(c => if c == '\t' then " ".repeat(KILO_TAB_STOP) else c.toChar.toString).mkString

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
    for
      _ <- StateT.modify[F, EditorConfig] { e =>
        key match
          case AKey.Left =>
            if e.cx != 0 then e.copy(cx = e.cx - 1)
            else if e.cy > 0 then e.copy(cy = e.cy - 1, cx = e.rows(e.cy - 1).chars.size)
            else e
          case AKey.Right =>
            e.rows
              .lift(e.cy)
              .fold(e)(r =>
                if e.cx < r.chars.size then e.copy(cx = e.cx + 1)
                else if e.cx == r.chars.size then e.copy(cy = e.cy + 1, cx = 0)
                else e
              )
          case AKey.Up   => if e.cy != 0 then e.copy(cy = e.cy - 1) else e
          case AKey.Down => if e.cy < e.rows.size then e.copy(cy = e.cy + 1) else e
      }
      _ <- StateT.modify[F, EditorConfig] { e =>
        val rowLen = e.rows.lift(e.cy).fold(0)(_.chars.size)
        e.copy(cx = e.cx `min` rowLen)
      }
    yield ()

  def editorInsertChar[F[_]: MonadThrow](char: Byte): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { c =>
      val newConfig = c.rows.lift(c.cy) match
        case Some(row) =>
          row.chars.insert(c.cx, char)
          c.rows.update(c.cy, row.copy(render = editorUpdateRow(row.chars)))
          c
        case None =>
          c.rows.append(Row(ArrayBuffer(char), editorUpdateRow(ArrayBuffer(char))))
          c
      newConfig.copy(cx = newConfig.cx + 1)
    }

  def editorProcessKeypress[F[_]: MonadThrow: Defer](): EditorConfigState[F, EitherRawResult[Unit]] =
    val successState = Right(()).pure[EditorConfigState[F, *]]
    def failedState(exitCode: Int) = Left(exitCode).pure[EditorConfigState[F, *]]
    for
      k <- StateT.liftF(editorReadKey())
      config <- StateT.get
      r: EitherRawResult[Unit] <- k match
        case Char(EXIT) => StateT.liftF(resetScreenCursorTask) >> failedState(0)
        case Home       => StateT.modify[F, EditorConfig](_.copy(cx = 0)) >> successState
        case End =>
          StateT.modify[F, EditorConfig](e =>
            if e.cy < e.rows.size then e.copy(cx = e.screenCols - 1) else e
          ) >> successState
        case Arrow(a) =>
          editorMoveCursor(a) >> successState
        case Page(a) =>
          StateT.modify[F, EditorConfig] { e =>
            val cy = a match
              case Up   => e.rowoff
              case Down => e.rowoff + e.screenRows - 1 `min` e.rows.size
            e.copy(cy = cy)
          }
            >> editorMoveCursor(if a == Up then AKey.Up else AKey.Down).replicateA_(config.screenRows) >> successState
        case Char(c) => editorInsertChar(c) >> successState
        case _ => successState
    yield r
    end for
  end editorProcessKeypress

  def editorDrawRows[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
    def appendLineBreak: String =
      s"${eraseInLine.esc}\r\n"

    StateT.modify[F, StringBuilder](bldr =>
      config.rows
        .map(_.some)
        .drop(config.rowoff)
        .take(config.screenRows)
        .padTo(config.screenRows, None)
        .zipWithIndex
        .foldLeft(bldr)((bldr, v) =>
          bldr ++= (v match
            case (Some(row), idx) =>
              val len = row.render.size - config.coloff `max` 0
              row.render
                .drop(config.coloff)
                .slice(0, if len > config.screenCols then config.screenCols else len)
                .map(_.toChar)
                .mkString
                ++ appendLineBreak
            // s"Test $idx ${config.rowoff} ${config.screenRows} cy=${config.cy}"
            //   ++ appendLineBreak(idx)
            case (None, idx) =>
              (if config.rows.isEmpty && idx == (config.screenRows / 3) then
                 val welcomeDisplayLen = if welcome.size > config.screenCols then config.screenCols else welcome.size
                 val padding = (config.screenCols - welcomeDisplayLen) / 2
                 (if padding > 0 then "~" else "")
                   + (if padding - 1 > 0 then " ".repeat(padding - 1) else "")
                   + welcome.substring(0, welcomeDisplayLen)
               else "~")
              ++ appendLineBreak
          )
        )
      bldr
    )
  end editorDrawRows

  def editorDrawStatusBar[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
    StateT.modify[F, StringBuilder](bldr =>
      val fileStatusStr = s"${config.filename.fold("[No Name]")(_.slice(0, 20))} - ${config.rows.size} lines"
      val currentRowColStr = s"${config.cy + 1}/${config.rows.size}"
      val blankSize = config.screenCols - fileStatusStr.size
      bldr ++= "[7m".esc + fileStatusStr
        + (if blankSize >= currentRowColStr.size then " " * (blankSize - currentRowColStr.size) else " " * blankSize)
        + (if blankSize >= currentRowColStr.size then currentRowColStr else "")
        + "[m".esc
        + "\r\n"
    )
  end editorDrawStatusBar

  def editorDrawMessageBar[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
    StateT.modify[F, StringBuilder](bldr =>
      bldr ++= eraseInLine.esc + config.statusMsg.fold("")(statusMsg =>
        val msgLen = statusMsg.msg.size `min` config.screenCols
        if Instant.now().getEpochSecond() - statusMsg.time.getEpochSecond() < 5 then statusMsg.msg.substring(0, msgLen)
        else ""
      )
    )

  def editorRefreshScreen[F[_]: MonadThrow](config: EditorConfig): F[Unit] =
    def setCursoer = s"[${(config.cy - config.rowoff) + 1};${config.rx - config.coloff + 1}H"
    for r <- Ref[F, String]("").pure
    yield ()
    val a = for
      _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStr(hideCursor, resetCursor))
      _ <- editorDrawRows(config)
      _ <- editorDrawStatusBar(config)
      _ <- editorDrawMessageBar(config)
      _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStrR(setCursoer, showCursor))
      bldr <- StateT.get
      _ <- StateT.liftF({
        val s = bldr.toString()
        print(s)
      }.pure)
    yield ()
    a.run(StringBuilder.newBuilder).map(_._2)
  end editorRefreshScreen

  def editorRowCxToRx(row: Row, cx: Int): Int =
    row.chars.take(cx).foldLeft(0)((r, c) => r + (if c == '\t' then KILO_TAB_STOP - (r % KILO_TAB_STOP) else 1))

  def editorScroll[F[_]: MonadThrow]: EditorConfigState[F, Unit] =
    StateT.modify(c =>
      val rx = c.cy match
        case cy if cy < c.rows.size => editorRowCxToRx(c.rows(cy), c.cx)
        case _                      => 0
      val rowoff = c.cy match
        case cy if cy < c.rowoff                 => cy
        case cy if cy >= c.rowoff + c.screenRows => cy - c.screenRows + 1
        case _: Int                              => c.rowoff
      val coloff = rx match
        case rx if rx < c.coloff                 => rx
        case rx if rx >= c.coloff + c.screenCols => rx - c.screenCols + 1
        case _: Int                              => c.coloff
      c.copy(rowoff = rowoff, coloff = coloff, rx = rx)
    )

  def program[F[_]: MonadThrow: Defer](filenameOpt: Option[String]): EditorConfigState[F, Unit] =
    def go: EditorConfigState[F, Unit] =
      val a = for
        _ <- editorScroll
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
      _ <- editorOpen(filenameOpt)
      _ <- go
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        program[Task](args.headOption).run(EditorConfig(0, 0, 0, 0, 0, 0, 0, StatusMessage(KILO_MSG).some)).map(_._2)
      )
      .handleErrorWith(e =>
        resetScreenCursorTask[Task] >>
          Task.apply(
            printf(f"%%s\n", e.getMessage())
          )
      )
      .asIO
      .void
end Main
