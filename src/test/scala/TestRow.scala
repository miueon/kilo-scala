import cats.syntax.all.*
import Row.*

class TestRow extends munit.FunSuite:
  test("test update syntax") {
    val line = "case case"
    val row = Row(line.getBytes().toList.toVector)
    val syntaxConfig = SyntaxConfig(
      hightlightNumbers = true,
      highlightSlStrs = true,
      slCommentStart = Vector("//"),
      mlCommentDelim = ("/*", "*/").some,
      keywords = Vector((HighlightType.Keyword1(), Vector("def", "case", "catch")))
    )
    val (updatedRow, hlState) = row.update(syntaxConfig, HLState.Normal)
    println(updatedRow.chars.map(_.toChar).mkString)
    println(updatedRow.hl.map(_.color).mkString(", "))
    println(hlState)
  }
