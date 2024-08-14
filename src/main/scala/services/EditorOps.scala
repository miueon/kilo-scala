package services

import `macro`.*
import cats.Defer
import cats.MonadThrow
import cats.data.StateT
import cats.mtl.Stateful
import cats.syntax.all.*
import domain.*
import domain.EditorConfig.*
import effect.Ref
import os.Path
import rawmode.*
import rawmode.all.*
import util.Utils.*

import scala.scalanative.posix.sys.ioctl
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.util.Try

type EitherRawResult[A] = Either[Int, A]

trait EditorOps[F[_]]:
  def updateWindowsSize: F[Unit]
  def scroll: F[Unit]
  def updateStatusMsg(strOpt: Option[String]): F[Unit]
  def refreshScreen(config: EditorConfig): F[Unit]
  def readKey: F[Key]
  def processKeypress(k: Key): F[EitherRawResult[Unit]]
  def processPromptKeypress(p: PromptMode, k: Key): F[EitherRawResult[Unit]]
  def openFile(filenameOpt: Option[String]): F[Unit]

type EditorConfigState[F[_]] = Stateful[F, EditorConfig]
object EditorConfigState:
  def apply[F[_]: EditorConfigState] = summon[EditorConfigState[F]]

object EditorOps:
  import Key.*
  import PageKey.*
  val wd = os.pwd
  def make[F[_]: MonadThrow: Defer: EditorConfigState](syntaxConfigOps: SyntaxConfigOps[F]): EditorOps[F] =
    new:
      def openFile(filenameOpt: Option[String]): F[Unit] =
        filenameOpt.fold((()).pure)(filename =>
          for
            _ <- loadSyntaxHighlight(wd / filename)
            contents <- os.read.lines(wd / filename).pure
            rows = contents
              .map(r =>
                val arr = Vector(r.removeSuffixNewLine.getBytes*)
                Row(arr)
              )
              .to(Vector)
            _ <- EditorConfigState[F].modify(c =>
              c.copy(
                rows = rows.updateAllRowsSyntaxAndRender(c.syntax),
                filename = filename.some
              )
            )
          yield ()
        )

      def processPromptKeypress(p: PromptMode, k: Key): F[EitherRawResult[Unit]] =
        val exitPromptMode = updatePromptMode(None) >> successState
        val result = p match
          case PromptMode.Save(str) =>
            PromptMode.nextStateByKeypress(str, k) match
              case PromptState.Active(str) =>
                updatePromptMode(PromptMode.Save(str).some) >> successState
              case PromptState.Cancelled =>
                updateStatusMsg("Save aborted".some) >> exitPromptMode
              case PromptState.Completed(str) => saveAs(str) >> exitPromptMode
          case PromptMode.Find(str, cursor, lastMatch) =>
            EditorConfigState[F].modify { c =>
              lastMatch.fold(c)(idx => c.copy(rows = c.rows.updated(idx, c.rows(idx).copy(matchSegment = None))))
            } >> {
              PromptMode.nextStateByKeypress(str, k) match
                case PromptState.Active(query) =>
                  val (lastMatch1, isForward) = k match
                    case Arrow(AKey.Right) | Arrow(AKey.Down) | Char(FIND) => (lastMatch, true)
                    case Arrow(AKey.Left) | Arrow(AKey.Up)                 => (lastMatch, false)
                    case _                                                 => (None, true)
                  find(query, cursor, lastMatch1, isForward) >> successState

                case PromptState.Cancelled    => updateCursor(cursor) >> exitPromptMode
                case PromptState.Completed(_) => exitPromptMode
            }
          case _ => ???
        for
          _ <- updateStatusMsg(None)
          r <- result
        yield r
      end processPromptKeypress
      def processKeypress(k: Key): F[EitherRawResult[Unit]] =
        for
          config <- EditorConfigState[F].get
          r: EitherRawResult[Unit] <- k match
            case Char(EXIT) =>
              if config.dirty && config.quitTimes > 0 then
                EditorConfigState[F].modify(c =>
                  c.copy(
                    quitTimes = c.quitTimes - 1,
                    statusMsg = StatusMessage(
                      s"WARNING!!! File has unsaved changes. Press Ctrl-Q ${c.quitTimes} more times to quit."
                    ).some
                  )
                ) >> exitSuccessState
              else resetScreenCursor >> exitState(0)
            case Char('\r')                          => insertNewLine >> successState
            case Char(BACKSPACE) | Char(DELETE_BITS) => deleteChar >> successState
            case Delete                              => moveCursor(AKey.Right) >> deleteChar >> successState
            case Char(REFRESH_SCREEN) | Escape       => successState
            case Char(SAVE) =>
              config.filename.fold(updatePromptMode(PromptMode.Save("").some))(filename =>
                saveAndHandleErrors(filename).void
              )
                >> successState
            case Char(FIND) =>
              updatePromptMode(PromptMode.Find("", CursorState.make(config)).some) >> successState
            case Home => EditorConfigState[F].modify(_.copy(cx = 0)) >> successState
            case End =>
              EditorConfigState[F].modify(e => e.copy(cx = e.currentRow.map(_.chars.size).getOrElse(0))) >> successState
            case Arrow(a) =>
              moveCursor(a) >> successState
            case Page(a) =>
              EditorConfigState[F].modify { e =>
                val cy = a match
                  case Up   => e.rowoff `saturatingSub` e.screenRows
                  case Down => (e.rowoff + 2 * e.screenRows - 1) `min` e.rows.size
                e.copy(cy = cy)
              }
                >> updateCursorXPosition >> successState
            case Char(c) => insertChar(c) >> successState
            case _       => successState
        yield r
        end for
      end processKeypress
      
      def readKey: F[Key] =
        def readUntil = Zone {
          def go(cPtrRef: Ref[F, Ptr[CChar]]): F[CChar] =
            for
              cPtr <- cPtrRef.get
              nread <- unistd.read(unistd.STDIN_FILENO, cPtr, 1.toUInt).pure
              result <- nread match
                case -1 => MonadThrow[F].raiseError(new Exception("read"))
                case 1  => (!cPtr).pure
                case _  => go(cPtrRef)
            yield result
          val ref = Ref[F, Ptr[CChar]](alloc())
          go(ref)
        }
        def readFollowingKey =
          val a = stackalloc[CChar]()
          if unistd.read(unistd.STDIN_FILENO, a, 1.toUInt) != 1 then None
          else (!a).some

        def readArrow(c: Option[Byte], d: Option[Byte]): Key =
          (c, d) match
            case (Some('1' | '7'), Some('~')) => Home
            case (Some('4' | '8'), Some('~')) => End
            case (Some('3'), Some('~'))       => Delete
            case (Some('5'), Some('~'))       => Page(Up)
            case (Some('6'), Some('~'))       => Page(Down)
            case (Some('5'), Some('A'))       => CtrlArrow(AKey.Up)
            case (Some('5'), Some('B'))       => CtrlArrow(AKey.Down)
            case (Some('5'), Some('C'))       => CtrlArrow(AKey.Right)
            case (Some('5'), Some('D'))       => CtrlArrow(AKey.Left)
            case _                            => Escape
        for
          a <- readUntil
          r <-
            if a == escInt.toByte then
              Defer[F].defer {
                {
                  readFollowingKey match
                    case Some('[') =>
                      readFollowingKey match
                        case Some('A') => Arrow(AKey.Up)
                        case Some('B') => Arrow(AKey.Down)
                        case Some('C') => Arrow(AKey.Right)
                        case Some('D') => Arrow(AKey.Left)
                        case Some('H') => Home
                        case Some('F') => End
                        case c @ Some(cv) if cv >= '0' && cv <= '9' =>
                          val d = readFollowingKey
                          (c, d) match
                            case (Some('1'), Some(';')) => readArrow(readFollowingKey, readFollowingKey)
                            case _                      => readArrow(c, d)
                        case _ => Escape
                    case Some('0') =>
                      readFollowingKey match
                        case Some('a') => CtrlArrow(AKey.Up)
                        case Some('b') => CtrlArrow(AKey.Down)
                        case Some('c') => CtrlArrow(AKey.Right)
                        case Some('d') => CtrlArrow(AKey.Left)
                        case _         => Escape
                    case _ => Escape
                  end match
                }.pure
              }
            else Char(a).pure
        yield r
        end for
      end readKey

      def refreshScreen(config: EditorConfig): F[Unit] =
        def setCursor = s"[${(config.cy - config.rowoff) + 1};${config.rx - config.coloff + 1}H"
        val a = for
          _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStr(hideCursor, resetCursor))
          _ <- DrawOps.drawRows(config)
          _ <- DrawOps.drawStatusBar(config)
          _ <- DrawOps.drawMsgBar(config)
          _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStrR(setCursor, showCursor))
          bldr <- StateT.get
          _ <- StateT.liftF({
            val s = bldr.toString()
            print(s)
          }.pure)
        yield ()
        a.run(StringBuilder.newBuilder).map(_._2)

      def scroll: F[Unit] =
        EditorConfigState[F].modify(c =>
          val rx = c.cy match
            case cy if cy < c.rows.size => rowCxToRx(c.rows(cy), c.cx)
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

      def updateWindowsSize: F[Unit] =
        for
          result <- getWindowSize
          _ <- result match
            case Left(_) => MonadThrow[F].raiseError(new Exception("getWindowSize"))
            case Right((col, row)) =>
              EditorConfigState[F].modify(c =>
                val screenRow = row - 2
                c.copy(
                  screenRows = screenRow,
                  screenCols = col,
                  cx = if c.cx > col then col - 1 else c.cx,
                  cy = if c.cy > screenRow then screenRow - 1 else c.cy
                )
              )
        yield ()

      def updateStatusMsg(strOpt: Option[String]): F[Unit] =
        EditorConfigState[F].modify(_.copy(statusMsg = strOpt.map(StatusMessage(_))))

      private def getWindowSize: F[EitherRawResult[(Int, Int)]] =
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

      private def rowCxToRx(row: Row, cx: Int): Int =
        row.chars.take(cx).foldLeft(0)((r, c) => r + (if c == '\t' then KILO_TAB_STOP - (r % KILO_TAB_STOP) else 1))

      private def updatePromptMode(p: Option[PromptMode]): F[Unit] =
        EditorConfigState[F].modify(_.copy(promptMode = p))

      private def successState: F[EitherRawResult[Unit]] =
        EditorConfigState[F].modify(_.copy(quitTimes = KILO_QUIT_TIMES)) >> Right(()).pure

      private def exitSuccessState: F[EitherRawResult[Unit]] = Right(()).pure

      private def exitState(exitCode: Int): F[EitherRawResult[Unit]] = Left(exitCode).pure

      private def saveAs(filename: String): F[Unit] =
        for
          r <- saveAndHandleErrors(filename)
          _ <- loadSyntaxHighlight(wd / filename)
          _ <- EditorConfigState[F].modify(c =>
            val rows = c.rows.updateAllRowsSyntaxAndRender(c.syntax)
            c.copy(rows = rows, filename = if r then filename.some else c.filename)
          )
        yield ()

      private def loadSyntaxHighlight(path: Path): F[Unit] =
        val ext = path.ext
        for
          c <- EditorConfigState[F].get
          updatedConfig <-
            if ext.isBlank() then c.pure
            else
              syntaxConfigOps.load(ext).map {
                case Some(syntax) => c.copy(syntax = syntax)
                case None         => c
              }
          _ <- EditorConfigState[F].set(updatedConfig)
        yield ()
      end loadSyntaxHighlight

      private def saveAndHandleErrors(filename: String): F[Boolean] =
        for
          config <- EditorConfigState[F].get
          content <- config.rows.renderToString.pure
          result <- saveContentToFile(filename, content)
          _ <- EditorConfigState[F].modify { c =>
            result.fold(
              e => c.copy(statusMsg = StatusMessage(s"Can't save! I/O error: ${e.getMessage()}").some),
              _ => c.copy(statusMsg = StatusMessage(s"${content.size} bytes written to disk").some, dirty = false)
            )
          }
        yield result.isRight

      private def find(
          query: String,
          savedCursor: CursorState,
          lastMatch: Option[Int],
          isForward: Boolean
      ): F[Unit] =
        type CurPOS = Int
        type RowPOS = Int
        @annotation.tailrec
        def go(rows: Vector[Row], pos: Int, idx: Int): Option[(CurPOS, RowPOS)] =
          if idx >= rows.size then None
          else
            val curPos = (pos + (if isForward then 1 else rows.size - 1)) % rows.size
            val row = rows(curPos)
            val matchRowPos = row.render.indexOf(query)
            if matchRowPos != -1 then (curPos, matchRowPos).some
            else go(rows, curPos, idx + 1)
        for
          config <- EditorConfigState[F].get
          numRows = config.rows.size
          current = lastMatch.getOrElse(numRows `saturatingSub` 1)
          _ <- go(config.rows, current, 0).fold(updatePromptMode(PromptMode.Find(query, savedCursor, None).some))(
            (curPOS, rowPOS) =>
              EditorConfigState[F].modify { c =>
                c.copy(
                  cy = curPOS,
                  cx = config.rows(curPOS).rx2cx(rowPOS),
                  coloff = 0,
                  rows =
                    c.rows.updated(curPOS, c.rows(curPOS).copy(matchSegment = (rowPOS until rowPOS + query.size).some))
                )
              }
                >> updatePromptMode(PromptMode.Find(query, savedCursor, curPOS.some).some)
          )
        yield ()
      end find

      private def updateCursor(cursor: CursorState): F[Unit] =
        EditorConfigState[F].modify(_.copy(cx = cursor.x, cy = cursor.y, rowoff = cursor.roff, coloff = cursor.coff))

      private def saveContentToFile(filename: String, content: String): F[Either[Throwable, Unit]] =
        Try(
          os.write.over(
            wd / filename,
            content,
            truncate = true
          )
        ).toEither.pure
      end saveContentToFile


      private def insertNewLine: F[Unit] =
        EditorConfigState[F].modify { c =>
          val addedNewLineRows =
            if c.cx == 0 then c.rows.patch(c.cy, List(Row(Vector.empty)), 0)
            else
              val row = c.rows(c.cy)
              val updatedChars = row.chars.take(c.cx)
              val updated = c.rows.updated(c.cy, row.copy(chars = updatedChars))
              val updatedSyntax = updated.updateRowSyntaxAndRender(c.cy, c.syntax, true)
              val newChars = row.chars.drop(c.cx)
              val newRow = Row(newChars)
              val inserted = updatedSyntax.patch(c.cy + 1, List(newRow), 0)
              inserted.updateRowSyntaxAndRender(c.cy + 1, c.syntax, false)
          c.copy(
            rows = addedNewLineRows,
            cy = c.cy + 1,
            cx = 0,
            dirty = true
          )
        }

      private def deleteChar: F[Unit] =
        for
          _ <- EditorConfigState[F].modify { c =>
            val newConfig = c.rows.lift(c.cy) match
              case Some(row) =>
                if c.cx == 0 && c.cy == 0 then c
                else if c.cx > 0 then
                  val removedCxChars = row.chars.patch(c.cx - 1, Nil, 1)
                  val updatedRows = c.rows.updated(
                    c.cy,
                    row.copy(chars = removedCxChars)
                  )
                  val updatedSyntax = updatedRows.updateRowSyntaxAndRender(c.cy, c.syntax, false)
                  c.copy(cx = c.cx - 1, dirty = true, rows = updatedSyntax)
                else if c.cx == 0 then
                  val removedCyRows = c.rows.patch(c.cy, Nil, 1)
                  val prevRow = c.rows(c.cy - 1)
                  val prevRowLen = prevRow.chars.size
                  val appendedPrevRowChars = prevRow.chars.appendedAll(row.chars)
                  val updatedRows = removedCyRows.updated(
                    c.cy - 1,
                    prevRow.copy(chars = appendedPrevRowChars)
                  )
                  val updatedSyntax = updatedRows.updateRowSyntaxAndRender(c.cy - 1, c.syntax, true)
                  c.copy(
                    rows = updatedSyntax,
                    cy = c.cy - 1,
                    cx = prevRowLen,
                    dirty = true
                  )
                else c
              case None => c
            newConfig
          }
          config <- EditorConfigState[F].get
          _ <-
            if config.cy == config.rows.size then moveCursor(AKey.Left)
            else ().pure
        yield ()
        end for
      end deleteChar

      private def moveCursor(key: AKey): F[Unit] =
        for
          _ <- EditorConfigState[F].modify { e =>
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
          _ <- updateCursorXPosition
        yield ()

      private def updateCursorXPosition: F[Unit] =
        EditorConfigState[F].modify { e =>
          e.copy(cx = e.cx `min` e.currentRow.map(_.chars.size).getOrElse(0))
        }

      private def insertChar(char: Byte): F[Unit] =
        EditorConfigState[F].modify { c =>
          val updatedRows = c.rows.lift(c.cy) match
            case Some(row) =>
              val insertedChars = row.chars.patch(c.cx, List(char), 0)
              c.rows.updated(c.cy, row.copy(chars = insertedChars))
            case None =>
              c.rows.appended(Row(Vector(char)))
          val updatedSyntaxRows = updatedRows.updateRowSyntaxAndRender(c.cy, c.syntax, false)
          c.copy(cx = c.cx + 1, dirty = true, rows = updatedSyntaxRows)
        }

  inline def resetScreenCursor[F[_]: MonadThrow] = print(resetScreenCursorStr).pure
end EditorOps
