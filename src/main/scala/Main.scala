import `macro`.*
import cats.Defer
import cats.MonadThrow
import cats.data.State
import cats.data.StateT
import cats.data.Validated
import cats.syntax.all.*
import effect.*
import os.Path
import rawmode.*
import rawmode.all.*
import util.Utils.*

import java.nio.file.NoSuchFileException
import java.time.Instant
import scala.scalanative.posix.sys.ioctl
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.util.Try
import scala.util.boundary
import scala.util.boundary.Label
import scala.util.boundary.break

inline val KILO_VERSION = "0.0.1"
inline val KILO_TAB_STOP = 2
inline val KILO_MSG = "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find"
inline val KILO_QUIT_TIMES = 3
inline val KILO_SYNTAX_DIR = "syntax"
inline val escInt = 0x1b
inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
inline val BACKSPACE = 127
val EXIT = ctrlKey('q')
val DELETE_BITS = ctrlKey('h')
val REFRESH_SCREEN = ctrlKey('l')
val SAVE = ctrlKey('s')
val FIND = ctrlKey('f')
val GOTO = ctrlKey('g')
inline val hideCursor = "[?25l"
inline val showCursor = "[?25h"
inline val clearScreen = "[2J"
inline val eraseInLine = "[K"
inline val resetCursor = "[H"
inline val resetFmt = "[0m"
inline val reverseVideo = "[7m"
inline val welcome = "Kilo editor -- version " + KILO_VERSION
inline def resetScreenCursorStr = escJoinStr(clearScreen, resetCursor)
def escJoinStrR(xs: String*): String = xs.toSeq.mkString(esc, esc, "")

enum HighlightType:
  def color: Int
  case Normal(color: Int = 39)
  case Number(color: Int = 31)
  case Match(color: Int = 46)
  case Str(color: Int = 32)
  case MLStr(color: Int = 132)
  case Comment(color: Int = 34)
  case MLComment(color: Int = 134)
  case Keyword1(color: Int = 33)
  case Keyword2(color: Int = 35)

  override def toString(): String = 
    s"\u001b[${color}m"


case class SyntaxConfig(
    name: String = "",
    hightlightNumbers: Boolean = false,
    highlightSlStrs: Boolean = false,
    slCommentStart: Vector[String] = Vector.empty,
    mlCommentDelim: Option[(String, String)] = None,
    mlStrDelim: Option[String] = None,
    keywords: Vector[(HighlightType, Vector[String])] = Vector.empty
)

object SyntaxConfig:
  def load[F[_]: MonadThrow](ext: String): F[Option[SyntaxConfig]] =
    def go(idx: Int, entries: IndexedSeq[Path]): F[Option[SyntaxConfig]] =
      if idx >= entries.size then none.pure
      else
        val entry = entries(idx)
        for
          lines <- os.read.lines(entry).pure[F]
          (syntaxConfig, exts) <- parseSyntaxConfig(lines)
          result <- if exts.contains(ext) then syntaxConfig.some.pure[F] else go(idx + 1, entries)
        yield result
    val readResult = for
      entries <- os.list(wd / KILO_SYNTAX_DIR).pure[F]
      syntaxConfigOpt <- go(0, entries)
    yield syntaxConfigOpt
    readResult.handleErrorWith(e =>
      if e.isInstanceOf[NoSuchFileException] then None.pure else MonadThrow[F].raiseError(e)
    )

  def parseSyntaxConfig[F[_]: MonadThrow](lines: IndexedSeq[String]): F[(SyntaxConfig, List[String])] =
    lines.toList.traverse(parseLine).flatMap { validatedConfigs =>
      val (errors, r) = validatedConfigs.separate
      if errors.nonEmpty then (new Exception(errors.mkString(", "))).raiseError
      else
        val (config, exts) = r.separate
        val mergedConfig = config.fold(SyntaxConfig())((acc, conf) =>
          acc.copy(
            name = if conf.name.nonEmpty then conf.name else acc.name,
            hightlightNumbers = conf.hightlightNumbers || acc.hightlightNumbers,
            highlightSlStrs = conf.highlightSlStrs || acc.highlightSlStrs,
            slCommentStart = conf.slCommentStart.appendedAll(acc.slCommentStart),
            mlCommentDelim = conf.mlCommentDelim.orElse(acc.mlCommentDelim),
            mlStrDelim = conf.mlStrDelim.orElse(acc.mlStrDelim),
            keywords = conf.keywords.appendedAll(acc.keywords)
          )
        )
        (mergedConfig, exts.flatten).pure
    }
  end parseSyntaxConfig

  def parseLine[F[_]: MonadThrow](line: String): F[Validated[String, Either[SyntaxConfig, List[String]]]] =
    val parts = line.split("=", 2)
    (parts.lift(0), parts.lift(1)) match
      case (Some(commentLine), _) if commentLine.startsWith("#") || commentLine.startsWith(";") =>
        SyntaxConfig().asLeft.valid.pure
      case (Some(k), Some(v))           => parseKeyValue(k, v)
      case (Some(""), None) | (None, _) => SyntaxConfig().asLeft.valid.pure
      case (Some(_), None)              => "Invalid line: $line".invalid.pure

  def parseKeyValue[F[_]: MonadThrow](
      key: String,
      value: String
  ): F[Validated[String, Either[SyntaxConfig, List[String]]]] =
    key match
      case "name"       => SyntaxConfig(name = value).asLeft.valid.pure
      case "extensions" => value.split(",").map(_.trim).toList.asRight.valid.pure
      case "highlight_numbers" =>
        parseBool(value).map(v => SyntaxConfig(hightlightNumbers = v).asLeft[List[String]].valid[String])
      case "highlight_strings" => parseBool(value).map(v => SyntaxConfig(highlightSlStrs = v).asLeft.valid)
      case "singleline_comment_start" =>
        SyntaxConfig(slCommentStart = value.split(",").map(_.trim).toVector).asLeft.valid.pure
      case "multiline_comment_delim" =>
        val delims = value.split(",").map(_.trim)
        if delims.length != 2 then "Expected 2 delimiters for multiline comment".invalid.pure
        else SyntaxConfig(mlCommentDelim = (delims(0), delims(1)).some).asLeft.valid.pure
      case "multiline_string_delim" => SyntaxConfig(mlStrDelim = value.some).asLeft.valid.pure
      case "keywords_1" =>
        SyntaxConfig(keywords =
          Vector((HighlightType.Keyword1(), value.split(",").map(_.trim).toVector))
        ).asLeft.valid.pure
      case "keywords_2" =>
        SyntaxConfig(keywords =
          Vector((HighlightType.Keyword2(), value.split(",").map(_.trim).toVector))
        ).asLeft.valid.pure
      case _ => s"Invalid key: $key".invalid.pure
  end parseKeyValue

  def parseBool[F[_]: MonadThrow](value: String): F[Boolean] =
    value.toLowerCase() match
      case "true" | "1" | "yes" => true.pure
      case "false" | "0" | "no" => false.pure
      case _                    => new IllegalArgumentException(s"Invalid boolean value: $value").raiseError
end SyntaxConfig

enum HLState:
  case Normal
  case MultiLineComment
  case Str(c: Byte)
  case MultiLineStr

case class Row(
    chars: Vector[Byte],
    render: String = "",
    hl: Vector[HighlightType] = Vector.empty,
    hlState: HLState = HLState.Normal,
    matchSegment: Option[Range] = None
):
  def rx2cx(rx: Int): Int =
    var currentRx = 0
    boundary {
      chars.zipWithIndex.foreach { case (c, cx) =>
        currentRx += (if c == '\t' then KILO_TAB_STOP - (currentRx % KILO_TAB_STOP) else 1)
        if currentRx > rx then break(cx)
      }
      chars.length
    }

  def update(syntax: SyntaxConfig, previousHlState: HLState): (Row, HLState) =
    val render = chars.flatMap(c => if c == '\t' then " ".repeat(KILO_TAB_STOP) else c.toChar.toString).mkString
    updateSyntax(syntax, previousHlState).run(this.copy(render = render)).value

  def updateSyntax(syntax: SyntaxConfig, previousHlState: HLState): State[Row, HLState] =
    State { row =>
      val line = row.render.getBytes()
      def findSubStringInLine(i: Int, s: String): Boolean =
        line.slice(i, i + s.length).sameElements(s.getBytes())

      def isAsciiPunctuation(c: Byte) =
        c >= '!' && c <= '/' || c >= ':' && c <= '@' || c >= '[' && c <= '`' || c >= '{' && c <= '~'
      def isSeparator(c: Byte): Boolean = c.toChar.isWhitespace || c == '\u0000' || (isAsciiPunctuation(c) && c != '_')

      def updateHl(
          i: Int,
          currentHlState: HLState,
          currentHlTypes: Vector[HighlightType]
      ): (HLState, Vector[HighlightType]) =
        if i >= line.length then (currentHlState, currentHlTypes)
        else
          val newState: (HLState, HighlightType, Int) = currentHlState match
            case HLState.Normal =>
              syntax.slCommentStart.find(findSubStringInLine(i, _)) match
                case Some(commentStart) =>
                  return (HLState.Normal, currentHlTypes ++ Vector.fill(line.length - i)(HighlightType.Comment()))
                case None =>
                  syntax.mlCommentDelim
                    .flatMap { case (start, end) =>
                      if findSubStringInLine(i, start) then
                        (HLState.MultiLineComment, HighlightType.MLComment(), start.length).some
                      else None
                    }
                    .orElse {
                      syntax.mlStrDelim.flatMap { delim =>
                        if findSubStringInLine(i, delim) then
                          (HLState.MultiLineStr, HighlightType.MLStr(), delim.length).some
                        else None
                      }
                    }
                    .getOrElse {
                      if syntax.highlightSlStrs && (line(i) == '"' || line(i) == '\'') then
                        (HLState.Str(line(i)), HighlightType.Str(), 1)
                      else (currentHlState, HighlightType.Normal(), 1)
                    }
            case HLState.MultiLineComment =>
              syntax.mlCommentDelim
                .flatMap { case (_, end) =>
                  if findSubStringInLine(i, end) then (HLState.Normal, HighlightType.MLComment(), end.length).some
                  else None
                }
                .getOrElse((currentHlState, HighlightType.MLComment(), 1))

            case HLState.MultiLineStr =>
              syntax.mlStrDelim
                .flatMap { delim =>
                  if findSubStringInLine(i, delim) then (HLState.Normal, HighlightType.MLStr(), delim.length).some
                  else None
                }
                .getOrElse((currentHlState, HighlightType.MLStr(), 1))

            case HLState.Str(quote) =>
              if line(i) == quote then (HLState.Normal, HighlightType.Str(), 1)
              else if line(i) == '\\' && i < line.length - 1 then (currentHlState, HighlightType.Str(), 2)
              else (currentHlState, HighlightType.Str(), 1)

          val (nextState, hlType, advance) = newState
          val newHlTypes =  (
            if hlType == HighlightType.Normal() then
              val prevSep = i == 0 || isSeparator(line(i - 1))
              val isNumber = syntax.hightlightNumbers && (
                (line(i).toChar.isDigit && prevSep) || (currentHlTypes.lastOption.contains(
                  HighlightType.Number()
                ) && !prevSep && !isSeparator(line(i)))
              )
              if isNumber then Vector(HighlightType.Number())
              else if prevSep then
                syntax.keywords
                  .collectFirst {
                    case (keywordType, keywords)
                        if keywords.exists(kw =>
                          findSubStringInLine(i, kw) && (i + kw.length == line.length || isSeparator(
                            line(i + kw.length)
                          ))
                        ) =>
                      Vector.fill(keywords.find(findSubStringInLine(i, _)).get.length)(keywordType)
                  }
                  .getOrElse(Vector(HighlightType.Normal()))
              else Vector(HighlightType.Normal())
            else Vector.fill(advance)(hlType)
          )
          updateHl(i + newHlTypes.length, nextState, currentHlTypes ++ newHlTypes)
        end if
      end updateHl
      val (finalState, newHl) = updateHl(0, previousHlState, Vector.empty)
      val newHlState = finalState match
        case HLState.Str(_) => HLState.Normal
        case _              => finalState
      (row.copy(hl = newHl, hlState = newHlState), newHlState)
    }

  def drawRow(offset: Int, maxLen: Int): String =
    val bldr = StringBuilder()
    var currentHlType = HighlightType.Normal()
    render.zipWithIndex.drop(offset).take(maxLen).foreach { (c, i) =>
      var hlType = hl(i)
      if c.isControl then
        val renderedChar = if c <= 26 then ('@'.toInt + c.toInt).toChar else '?'
        bldr.append(s"${reverseVideo.esc}$renderedChar${resetFmt.esc}")
        if currentHlType != HighlightType.Normal then bldr.append(currentHlType)
      else
        matchSegment.foreach { range =>
          if range.contains(i) then hlType = HighlightType.Match()
          else if i == range.end then bldr.append(resetFmt.esc)
        }
        if currentHlType != hlType then
          bldr.append(hlType)
          currentHlType = hlType
        bldr.append(c)
    }
    bldr.append(resetFmt.esc)
    bldr.toString()
  end drawRow
end Row

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
    quitTimes: Int,
    dirty: Boolean,
    statusMsg: Option[StatusMessage] = None,
    syntax: SyntaxConfig = SyntaxConfig(),
    promptMode: Option[PromptMode] = None,
    rows: Vector[Row] = Vector.empty,
    filename: Option[String] = None
):
  def currentRow: Option[Row] = rows.get(cy)
end EditorConfig

object EditorConfig:
  extension (rows: Vector[Row])
    def updateRowSyntaxAndRender(y: Int, syntax: SyntaxConfig, isIgnoreFollowingRows: Boolean): Vector[Row] =
      @annotation.tailrec
      def go(idx: Int, rows: Vector[Row], hlState: HLState): Vector[Row] =
        if idx >= rows.size then rows
        else
          val previousHlState = rows(idx).hlState
          val (updatedRow, nextRowHlState) = rows(idx).update(syntax, hlState)
          val updatedRows = rows.updated(idx, updatedRow)
          if isIgnoreFollowingRows || nextRowHlState == previousHlState then updatedRows
          else go(idx + 1, updatedRows, nextRowHlState)

      val hlState = if y > 0 then rows(y - 1).hlState else HLState.Normal
      go(y, rows, hlState)

    def updateAllRowsSyntaxAndRender(syntax: SyntaxConfig): Vector[Row] =
      rows
        .foldLeft((HLState.Normal, Vector.empty[Row])) { case ((hlStateAcc, rowAcc), row) =>
          val (updatedRow, nextRowHlState) = row.update(syntax, hlStateAcc)
          (nextRowHlState, rowAcc.appended(updatedRow))
        }
        ._2
  end extension
end EditorConfig

case class CursorState(
    x: Int = 0,
    y: Int = 0,
    roff: Int = 0,
    coff: Int = 0
)

object CursorState:
  def make(e: EditorConfig) =
    CursorState(
      e.cx,
      e.cy,
      e.rowoff,
      e.coloff
    )

enum PromptMode:
  case Save(str: String)
  case Find(str: String, cursor: CursorState, lastMatch: Option[Int] = None)
  case GoTo(Str: String)

enum PromptState:
  case Active(str: String)
  case Completed(str: String)
  case Cancelled

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

import EditorConfig.*
object Main extends IOApp:

  def isAsciiControl(byte: Byte): Boolean =
    byte >= 0 && byte <= 31 || byte == 127
  def processPromptKeypress(str: String, k: Key): PromptState =
    k match
      case Char('\r')                                          => PromptState.Completed(str)
      case Key.Escape | Key.Char(EXIT)                         => PromptState.Cancelled
      case Key.Char(BACKSPACE) | Char(DELETE_BITS)             => PromptState.Active(str.slice(0, str.size - 1))
      case Char(c) if c >= 0 && c <= 126 && !isAsciiControl(c) => PromptState.Active(str + c.toChar)
      case _                                                   => PromptState.Active(str)

  def updateCursor[F[_]: MonadThrow](cursor: CursorState): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig](_.copy(cx = cursor.x, cy = cursor.y, rowoff = cursor.roff, coloff = cursor.coff))

  def updateStatusMsg[F[_]: MonadThrow](strOpt: Option[String]): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig](_.copy(statusMsg = strOpt.map(StatusMessage(_))))

  def updatePromptMode[F[_]: MonadThrow](p: Option[PromptMode]): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig](_.copy(promptMode = p))
  extension [F[_]: MonadThrow](p: PromptMode)
    def statusMsg: String =
      p match
        case PromptMode.Save(str)       => s"Save as: ${str}"
        case PromptMode.Find(str, _, _) => s"Search (Use ESC/Arrows/Enter): ${str}"
        case _                          => ???

    def processKeypress(k: Key): EditorConfigState[F, EitherRawResult[Unit]] =
      val exitPromptMode: EditorConfigState[F, EitherRawResult[Unit]] = updatePromptMode(None) >> successState
      val result = p match
        case PromptMode.Save(str) =>
          processPromptKeypress(str, k) match
            case PromptState.Active(str) =>
              updatePromptMode(PromptMode.Save(str).some) >> successState
            case PromptState.Cancelled =>
              updateStatusMsg("Save aborted".some) >> exitPromptMode
            case PromptState.Completed(str) => saveAs(str) >> exitPromptMode
        case PromptMode.Find(str, cursor, lastMatch) =>
          StateT.modify[F, EditorConfig] { c =>
            lastMatch.fold(c)(idx => c.copy(rows = c.rows.updated(idx, c.rows(idx).copy(matchSegment = None))))
          } >> {
            processPromptKeypress(str, k) match
              case PromptState.Active(query) =>
                val (lastMatch1, isForward) = k match
                  case Arrow(AKey.Right) | Arrow(AKey.Down) | Char(FIND) => (lastMatch, true)
                  case Arrow(AKey.Left) | Arrow(AKey.Up)                 => (lastMatch, false)
                  case _                                                 => (None, true)
                editorFind(query, cursor, lastMatch1, isForward) >> successState

              case PromptState.Cancelled    => updateCursor(cursor) >> exitPromptMode
              case PromptState.Completed(_) => exitPromptMode
          }
        case _ => ???
      for
        _ <- updateStatusMsg(None)
        r <- result
      yield r
    end processKeypress
  end extension

  def editorFind[F[_]: MonadThrow](
      query: String,
      savedCursor: CursorState,
      lastMatch: Option[Int],
      isForward: Boolean
  ): EditorConfigState[F, Unit] =
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
      config <- StateT.get[F, EditorConfig]
      numRows = config.rows.size
      current = lastMatch.getOrElse(numRows `saturatingSub` 1)
      _ <- go(config.rows, current, 0).fold(updatePromptMode(PromptMode.Find(query, savedCursor, None).some))(
        (curPOS, rowPOS) =>
          StateT.modify[F, EditorConfig] { c =>
            c.copy(
              cy = curPOS,
              cx = config.rows(curPOS).rx2cx(rowPOS),
              coloff = 0,
              rows = c.rows.updated(curPOS, c.rows(curPOS).copy(matchSegment = (rowPOS until rowPOS + query.size).some))
            )
          }
            >> updatePromptMode(PromptMode.Find(query, savedCursor, curPOS.some).some)
      )
    yield ()
  end editorFind

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

  def loadSyntaxHighlight[F[_]: MonadThrow](path: Path): EditorConfigState[F, Unit] =
    StateT.modifyF[F, EditorConfig](c =>
      val ext = path.ext
      if ext.isBlank() then c.pure
      else
        SyntaxConfig.load[F](ext).map {
          case Some(syntax) => c.copy(syntax = syntax)
          case None         => c
        }
    )

  def editorOpen[F[_]: MonadThrow](filenameOpt: Option[String]): EditorConfigState[F, Unit] =
    filenameOpt.fold[EditorConfigState[F, Unit]]((()).pure)(filename =>
      for
        _ <- loadSyntaxHighlight(wd / filename)
        contents <- StateT.liftF(os.read.lines(wd / filename).pure)
        rows = contents
          .map(r =>
            val arr = Vector(r.removeSuffixNewLine.getBytes*)
            Row(arr)
          )
          .to(Vector)
        _ <- StateT.modify[F, EditorConfig](c =>
          c.copy(
            rows = rows.updateAllRowsSyntaxAndRender(c.syntax),
            filename = filename.some
          )
        )
      yield ()
    )

  def editorRowToString(rows: Seq[Row]): String =
    rows.map(_.chars.map(_.toChar).mkString).mkString("\n")
  def saveContentToFile[F[_]: MonadThrow](filename: String, content: String): F[Either[Throwable, Unit]] =
    Try(
      os.write.over(
        wd / filename,
        content,
        truncate = true
      )
    ).toEither.pure
  end saveContentToFile

  def saveAndHandleErrors[F[_]: MonadThrow](filename: String): EditorConfigState[F, Boolean] =
    for
      config <- StateT.get[F, EditorConfig]
      content <- editorRowToString(config.rows).pure[EditorConfigState[F, *]]
      result <- StateT.liftF(saveContentToFile(filename, content))
      _ <- StateT.modify[F, EditorConfig] { c =>
        result.fold(
          e => c.copy(statusMsg = StatusMessage(s"Can't save! I/O error: ${e.getMessage()}").some),
          _ => c.copy(statusMsg = StatusMessage(s"${content.size} bytes written to disk").some, dirty = false)
        )
      }
    yield result.isRight

  def saveAs[F[_]: MonadThrow](filename: String): EditorConfigState[F, Unit] =
    for
      r <- saveAndHandleErrors(filename)
      _ <- loadSyntaxHighlight(wd / filename)
      _ <- StateT.modify[F, EditorConfig](c =>
        val rows = c.rows.updateAllRowsSyntaxAndRender(c.syntax)
        c.copy(rows = rows, filename = if r then filename.some else c.filename)
      )
    yield ()

  def editorRenderRow(arr: Seq[Byte]): String =
    arr.flatMap(c => if c == '\t' then " ".repeat(KILO_TAB_STOP) else c.toChar.toString).mkString

  def editorReadKey[F[_]: MonadThrow: Defer](): F[Key] =
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
      _ <- editorUpdateCursorXPosition
    yield ()

  def editorUpdateCursorXPosition[F[_]: MonadThrow]: EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { e =>
      e.copy(cx = e.cx `min` e.currentRow.map(_.chars.size).getOrElse(0))
    }

  def editorInsertChar[F[_]: MonadThrow](char: Byte): EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { c =>
      val updatedRows = c.rows.lift(c.cy) match
        case Some(row) =>
          val insertedChars = row.chars.patch(c.cx, List(char), 0)
          c.rows.updated(c.cy, row.copy(chars = insertedChars))
        case None =>
          c.rows.appended(Row(Vector(char)))
      val updatedSyntaxRows = updatedRows.updateRowSyntaxAndRender(c.cy, c.syntax, false)
      c.copy(cx = c.cx + 1, dirty = true, rows = updatedSyntaxRows)
    }

  def editorInsertNewLine[F[_]: MonadThrow]: EditorConfigState[F, Unit] =
    StateT.modify[F, EditorConfig] { c =>
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

  def editorDeleteChar[F[_]: MonadThrow]: EditorConfigState[F, Unit] =
    for
      _ <- StateT.modify[F, EditorConfig] { c =>
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
      config <- StateT.get
      _ <-
        if config.cy == config.rows.size then editorMoveCursor(AKey.Left)
        else ().pure[EditorConfigState[F, *]]
    yield ()
    end for
  end editorDeleteChar

  def exitSuccessState[F[_]: MonadThrow] = Right(()).pure[EditorConfigState[F, *]]
  def successState[F[_]: MonadThrow]: EditorConfigState[F, EitherRawResult[Unit]] =
    StateT.modify[F, EditorConfig](c => c.copy(quitTimes = KILO_QUIT_TIMES)) >>
      Right(()).pure[EditorConfigState[F, *]]
  def exitState[F[_]: MonadThrow](exitCode: Int) = Left(exitCode).pure[EditorConfigState[F, *]]

  def editorProcessKeypress[F[_]: MonadThrow: Defer](k: Key): EditorConfigState[F, EitherRawResult[Unit]] =
    for
      config <- StateT.get
      r: EitherRawResult[Unit] <- k match
        case Char(EXIT) =>
          if config.dirty && config.quitTimes > 0 then
            StateT.modify[F, EditorConfig](c =>
              c.copy(
                quitTimes = c.quitTimes - 1,
                statusMsg = StatusMessage(
                  s"WARNING!!! File has unsaved changes. Press Ctrl-Q ${c.quitTimes} more times to quit."
                ).some
              )
            ) >> exitSuccessState
          else StateT.liftF(resetScreenCursorTask) >> exitState(0)
        case Char('\r')                          => editorInsertNewLine >> successState
        case Char(BACKSPACE) | Char(DELETE_BITS) => editorDeleteChar >> successState
        case Delete                              => editorMoveCursor(AKey.Right) >> editorDeleteChar >> successState
        case Char(REFRESH_SCREEN) | Escape       => successState
        case Char(SAVE) =>
          config.filename.fold(updatePromptMode(PromptMode.Save("").some))(filename => saveAndHandleErrors(filename))
            >> successState
        case Char(FIND) =>
          updatePromptMode(PromptMode.Find("", CursorState.make(config)).some) >> successState
        case Home => StateT.modify[F, EditorConfig](_.copy(cx = 0)) >> successState
        case End =>
          StateT.modify[F, EditorConfig](e => e.copy(cx = e.currentRow.map(_.chars.size).getOrElse(0))) >> successState
        case Arrow(a) =>
          editorMoveCursor(a) >> successState
        case Page(a) =>
          StateT.modify[F, EditorConfig] { e =>
            val cy = a match
              case Up   => e.rowoff `saturatingSub` e.screenRows
              case Down => (e.rowoff + 2 * e.screenRows - 1) `min` e.rows.size
            e.copy(cy = cy)
          }
            >> editorUpdateCursorXPosition >> successState
        case Char(c) => editorInsertChar(c) >> successState
        case _       => successState
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
              row.drawRow(config.coloff, config.screenCols) ++ appendLineBreak
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
      val fileStatusStr = s"${config.filename
          .fold("[No Name]")(_.slice(0, 20))} - ${config.rows.size} lines ${if config.dirty then "(modified)" else ""}"
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
    def setCursor = s"[${(config.cy - config.rowoff) + 1};${config.rx - config.coloff + 1}H"
    val a = for
      _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStr(hideCursor, resetCursor))
      _ <- editorDrawRows(config)
      _ <- editorDrawStatusBar(config)
      _ <- editorDrawMessageBar(config)
      _ <- StateT.modify[F, StringBuilder](_ ++= escJoinStrR(setCursor, showCursor))
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
      for
        _ <- editorScroll
        config <- StateT.get[F, EditorConfig]
        _ <- config.promptMode.fold(().pure[EditorConfigState[F, *]])(p => updateStatusMsg(p.statusMsg.some))
        _ <- StateT.inspectF(editorRefreshScreen(_))
        k <- StateT.liftF(editorReadKey())
        r <- config.promptMode.fold(editorProcessKeypress(k))(_.processKeypress(k))
        _ <- r match
          case Left(v)   => StateT.liftF(MonadThrow[F].raiseError(new Exception(s"Exit code: $v")))
          case Right(()) => go
      yield ()
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
        program[Task](args.headOption)
          .run(EditorConfig(0, 0, 0, 0, 0, 0, 0, KILO_QUIT_TIMES, false, StatusMessage(KILO_MSG).some))
          .map(_._2)
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
