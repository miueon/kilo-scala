import cats.syntax.all.*
import domain.Row.*
import domain.*
import domain.HighlightType.*

class TestRow extends munit.FunSuite:
  test("test update syntax") {
    val line = "case case"
    val row = Row(line.getBytes().toList.toVector)
    val syntaxConfig = SyntaxConfig(
      highlightNumbers = true,
      highlightSlStrs = true,
      slCommentStart = Vector("//"),
      mlCommentDelim = ("/*", "*/").some,
      keywords = Vector((HighlightType.Keyword1(), Vector("def", "case", "catch")))
    )
    val (updatedRow, hlState) = row.update(syntaxConfig, HLState.Normal)
    assertEquals(
      updatedRow.hl,
      Vector.fill(4)(HighlightType.Keyword1()) ++ Vector(HighlightType.Normal()) ++ Vector.fill(4)(
        HighlightType.Keyword1()
      )
    )
    assertEquals(hlState, HLState.Normal)
  }
end TestRow
