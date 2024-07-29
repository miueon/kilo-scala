package domain
import scala.util.boundary
import scala.util.boundary.Label
import scala.util.boundary.break
import cats.data.State
import cats.syntax.all.*
import `macro`.*

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