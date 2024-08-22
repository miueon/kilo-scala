package domain

import java.time.Instant
import cats.syntax.all.*
import scala.scalanative.unsafe.*
import cats.mtl.Stateful

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

object PromptMode:
  def isAsciiControl(byte: Byte): Boolean =
    byte >= 0 && byte <= 31 || byte == 127

  import Key.*
  def nextStateByKeypress(str: String, k: Key): PromptState =
    k match
      case Char('\r')                                          => PromptState.Completed(str)
      case Key.Escape | Key.Char(EXIT)                         => PromptState.Cancelled
      case Key.Char(BACKSPACE) | Char(DELETE_BITS)             => PromptState.Active(str.slice(0, str.size - 1))
      case Char(c) if c >= 0 && c <= 126 && !isAsciiControl(c) => PromptState.Active(str + c.toChar)
      case _                                                   => PromptState.Active(str)

  extension (self: PromptMode)
    def statusMsg: String =
      self match
        case PromptMode.Save(str)       => s"Save as: ${str}"
        case PromptMode.Find(str, _, _) => s"Search (Use ESC/Arrows/Enter): ${str}"
        case _                          => ???
end PromptMode

enum PromptState:
  case Active(str: String)
  case Completed(str: String)
  case Cancelled

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
)

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

    def renderToString: String =
      rows.map(_.chars.map(_.toChar).mkString).mkString("\n")
  end extension

  extension (self: EditorConfig) def currentRow: Option[Row] = self.rows.get(self.cy)
end EditorConfig

type EditorConfigState[F[_]] = Stateful[F, EditorConfig]
object EditorConfigState:
  def apply[F[_]: EditorConfigState] = summon[EditorConfigState[F]]