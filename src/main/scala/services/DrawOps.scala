package services

import domain.*
import cats.MonadThrow
import cats.data.StateT
import cats.syntax.all.*
import `macro`.*
import java.time.Instant

object DrawOps:
  type EditorBufState[F[_]] = StateT[F, StringBuilder, Unit]
  def drawRows[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
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
  end drawRows

  def drawStatusBar[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
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

  def drawMsgBar[F[_]: MonadThrow](config: EditorConfig): EditorBufState[F] =
    StateT.modify[F, StringBuilder](bldr =>
      bldr ++= eraseInLine.esc + config.statusMsg.fold("")(statusMsg =>
        val msgLen = statusMsg.msg.size `min` config.screenCols
        if Instant.now().getEpochSecond() - statusMsg.time.getEpochSecond() < 5 then statusMsg.msg.substring(0, msgLen)
        else ""
      )
    )
end DrawOps
