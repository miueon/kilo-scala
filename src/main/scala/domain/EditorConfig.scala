package domain

import java.time.Instant
import cats.syntax.all.*

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