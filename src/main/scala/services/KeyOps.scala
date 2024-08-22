package services

import cats.Defer
import cats.MonadThrow
import cats.syntax.all.*
import domain.AKey
import domain.EditorConfig.*
import domain.EditorConfigState
import domain.Row

trait KeyOps[F[_]]:
  def moveCursor(key: AKey): F[Unit]
  def insertNewLine: F[Unit]
  def deleteChar: F[Unit]
  def insertChar(char: Byte): F[Unit]
  def updateCursorXPosition: F[Unit]

object KeyOps:
  def make[F[_]: MonadThrow: Defer: EditorConfigState]: KeyOps[F] = new:
    /** Deletes a character at the current cursor position.
      *
      * This method handles different scenarios:
      *   1. If at the beginning of the file, do nothing. 2. If in the middle of a row, remove the character before the
      *      cursor. 3. If at the beginning of a row (except the first row), merge with the previous row.
      *
      * After deletion, it updates the cursor position and syntax highlighting as needed.
      */
    def deleteChar: F[Unit] =
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

    def insertChar(char: Byte): F[Unit] =
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

    /** Inserts a new line at the current cursor position.
      *
      * This method handles two scenarios:
      *   1. If the cursor is at the beginning of a line, it inserts an empty row. 2. If the cursor is in the middle of
      *      a line, it splits the current line into two.
      *
      * After insertion, it updates the cursor position, marks the file as dirty, and updates syntax highlighting as
      * needed.
      */
    def insertNewLine: F[Unit] =
      EditorConfigState[F].modify { c =>
        val addedNewLineRows =
          if c.cx == 0 then
            // Insert an empty row at the current position
            c.rows.patch(c.cy, List(Row(Vector.empty)), 0)
          else
            val row = c.rows(c.cy)
            // Split the current row at the cursor position
            val updatedChars = row.chars.take(c.cx)
            val updated = c.rows.updated(c.cy, row.copy(chars = updatedChars))
            // Update syntax highlighting for the modified row
            val updatedSyntax = updated.updateRowSyntaxAndRender(c.cy, c.syntax, true)
            // Create a new row with the remaining characters
            val newChars = row.chars.drop(c.cx)
            val newRow = Row(newChars)
            // Insert the new row and update its syntax highlighting
            val inserted = updatedSyntax.patch(c.cy + 1, List(newRow), 0)
            inserted.updateRowSyntaxAndRender(c.cy + 1, c.syntax, false)
        // Update the editor configuration
        c.copy(
          rows = addedNewLineRows,
          cy = c.cy + 1, // Move cursor to the next line
          cx = 0, // Move cursor to the beginning of the new line
          dirty = true // Mark the file as modified
        )
      }
    /** Moves the cursor based on the given arrow key input.
      *
      * @param key
      *   The arrow key pressed (Left, Right, Up, or Down)
      * @return
      *   A monadic action that updates the editor state
      *
      * This method handles cursor movement in four directions:
      *   - Left: Moves cursor left, or to the end of the previous line if at the start of a line
      *   - Right: Moves cursor right, or to the start of the next line if at the end of a line
      *   - Up: Moves cursor up one line, if not already at the top
      *   - Down: Moves cursor down one line, if not already at the bottom
      *
      * After moving the cursor, it calls updateCursorXPosition to ensure the cursor is within bounds.
      */
    def moveCursor(key: AKey): F[Unit] =
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

    def updateCursorXPosition: F[Unit] =
      EditorConfigState[F].modify { e =>
        e.copy(cx = e.cx `min` e.currentRow.map(_.chars.size).getOrElse(0))
      }
end KeyOps
