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
)

object Row:
  extension (self: Row)
    def rx2cx(rx: Int): Int =
      var currentRx = 0
      boundary {
        self.chars.zipWithIndex.foreach { case (c, cx) =>
          currentRx += (if c == '\t' then KILO_TAB_STOP - (currentRx % KILO_TAB_STOP) else 1)
          if currentRx > rx then break(cx)
        }
        self.chars.length
      }

    def update(syntax: SyntaxConfig, previousHlState: HLState): (Row, HLState) =
      val render = self.chars.flatMap(c => if c == '\t' then " ".repeat(KILO_TAB_STOP) else c.toChar.toString).mkString
      updateSyntax(syntax, previousHlState).run(self.copy(render = render)).value

    def updateSyntax(syntax: SyntaxConfig, previousHlState: HLState): State[Row, HLState] =
      State { row =>
        val line = row.render.getBytes().toVector
        val (finalState, newHl) = updateHl(0, previousHlState, Vector.empty, line, syntax)
        val newHlState = finalizeHlState(finalState)
        (row.copy(hl = newHl, hlState = newHlState), newHlState)
      }

    def findSubStringInLine(line: Vector[Byte], i: Int, s: String): Boolean =
      line.slice(i, i + s.length).sameElements(s.getBytes())

    def isAsciiPunctuation(c: Byte): Boolean =
      c >= '!' && c <= '/' || c >= ':' && c <= '@' || c >= '[' && c <= '`' || c >= '{' && c <= '~'

    def isSeparator(c: Byte): Boolean =
      c.toChar.isWhitespace || c == '\u0000' || (isAsciiPunctuation(c) && c != '_')

    def updateHl(
        i: Int,
        currentHlState: HLState,
        currentHlTypes: Vector[HighlightType],
        line: Vector[Byte],
        syntax: SyntaxConfig
    ): (HLState, Vector[HighlightType]) =
      if i >= line.length then (currentHlState, currentHlTypes)
      else
        val (nextState, hlType, advance) = determineNewState(i, currentHlState, line, syntax)
        val newHlTypes = determineNewHlTypes(i, hlType, currentHlTypes, line, syntax, advance)
        updateHl(i + newHlTypes.length, nextState, currentHlTypes ++ newHlTypes, line, syntax)

    def determineNewState(i: Int, currentHlState: HLState, line: Vector[Byte], syntax: SyntaxConfig): (HLState, HighlightType, Int) =
      currentHlState match
        case HLState.Normal => handleNormalState(i, line, syntax)
        case HLState.MultiLineComment => handleMultiLineCommentState(i, line, syntax)
        case HLState.MultiLineStr => handleMultiLineStrState(i, line, syntax)
        case HLState.Str(quote) => handleStrState(i, line, quote)

    def handleNormalState(i: Int, line: Vector[Byte], syntax: SyntaxConfig): (HLState, HighlightType, Int) =
      syntax.slCommentStart.find(findSubStringInLine(line, i, _)) match
        case Some(_) => (HLState.Normal, HighlightType.Comment(), line.length - i)
        case None =>
          syntax.mlCommentDelim
            .flatMap { case (start, _) =>
              if findSubStringInLine(line, i, start) then
                (HLState.MultiLineComment, HighlightType.MLComment(), start.length).some
              else None
            }
            .orElse {
              syntax.mlStrDelim.flatMap { delim =>
                if findSubStringInLine(line, i, delim) then
                  (HLState.MultiLineStr, HighlightType.MLStr(), delim.length).some
                else None
              }
            }
            .getOrElse {
              if syntax.highlightSlStrs && (line(i) == '"' || line(i) == '\'') then
                (HLState.Str(line(i)), HighlightType.Str(), 1)
              else (HLState.Normal, HighlightType.Normal(), 1)
            }

    def handleMultiLineCommentState(i: Int, line: Vector[Byte], syntax: SyntaxConfig): (HLState, HighlightType, Int) =
      syntax.mlCommentDelim
        .flatMap { case (_, end) =>
          if findSubStringInLine(line, i, end) then (HLState.Normal, HighlightType.MLComment(), end.length).some
          else None
        }
        .getOrElse((HLState.MultiLineComment, HighlightType.MLComment(), 1))

    def handleMultiLineStrState(i: Int, line: Vector[Byte], syntax: SyntaxConfig): (HLState, HighlightType, Int) =
      syntax.mlStrDelim
        .flatMap { delim =>
          if findSubStringInLine(line, i, delim) then (HLState.Normal, HighlightType.MLStr(), delim.length).some
          else None
        }
        .getOrElse((HLState.MultiLineStr, HighlightType.MLStr(), 1))

    def handleStrState(i: Int, line: Vector[Byte], quote: Byte): (HLState, HighlightType, Int) =
      if line(i) == quote then (HLState.Normal, HighlightType.Str(), 1)
      else if line(i) == '\\' && i < line.length - 1 then (HLState.Str(quote), HighlightType.Str(), 2)
      else (HLState.Str(quote), HighlightType.Str(), 1)

    def determineNewHlTypes(i: Int, hlType: HighlightType, currentHlTypes: Vector[HighlightType], line: Vector[Byte], syntax: SyntaxConfig, advance: Int): Vector[HighlightType] =
      if hlType == HighlightType.Normal() then
        val prevSep = i == 0 || isSeparator(line(i - 1))
        val isNumber = isNumberHighlight(i, prevSep, currentHlTypes, line, syntax)
        if isNumber then Vector(HighlightType.Number())
        else if prevSep then determineKeywordHighlight(i, line, syntax)
        else Vector(HighlightType.Normal())
      else Vector.fill(advance)(hlType)

    def isNumberHighlight(i: Int, prevSep: Boolean, currentHlTypes: Vector[HighlightType], line: Vector[Byte], syntax: SyntaxConfig): Boolean =
      syntax.hightlightNumbers && (
        (line(i).toChar.isDigit && prevSep) || (currentHlTypes.lastOption.contains(HighlightType.Number()) && !prevSep && !isSeparator(line(i)))
      )

    def determineKeywordHighlight(i: Int, line: Vector[Byte], syntax: SyntaxConfig): Vector[HighlightType] =
      syntax.keywords
        .collectFirst {
          case (keywordType, keywords)
              if keywords.exists(kw =>
                findSubStringInLine(line, i, kw) && (i + kw.length == line.length || isSeparator(line(i + kw.length)))
              ) =>
            Vector.fill(keywords.find(findSubStringInLine(line, i, _)).get.length)(keywordType)
        }
        .getOrElse(Vector(HighlightType.Normal()))

    def finalizeHlState(state: HLState): HLState =
      state match
        case HLState.Str(_) => HLState.Normal
        case _              => state

    def drawRow(offset: Int, maxLen: Int): String =
      val bldr = StringBuilder()
      var currentHlType = HighlightType.Normal()
      self.render.zipWithIndex.drop(offset).take(maxLen).foreach { (c, i) =>
        var hlType = self.hl(i)
        if c.isControl then
          val renderedChar = if c <= 26 then ('@'.toInt + c.toInt).toChar else '?'
          bldr.append(s"${reverseVideo.esc}$renderedChar${resetFmt.esc}")
          if currentHlType != HighlightType.Normal() then bldr.append(currentHlType)
        else
          self.matchSegment.foreach { range =>
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
  end extension
end Row
