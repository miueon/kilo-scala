import domain.EditorConfig
import domain.Row
import domain.SyntaxConfig

class EditorOpsTest extends munit.FunSuite:
  test("deleteChar") {
    val editorConfig = EditorConfig(
      cx = 2,
      cy = 0,
      rx = 2,
      rowoff = 0,
      coloff = 0,
      screenRows = 24,
      screenCols = 80,
      quitTimes = 3,
      dirty = false,
      statusMsg = None,
      syntax = SyntaxConfig(),
      promptMode = None,
      rows = Vector(Row(chars = Vector('a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte))),
      filename = None
    )

    editorOps.deleteChar()
    val finalConfig = editorOps.editorConfigState.get
    assertEquals(finalConfig.cx, 2)
    assertEquals(finalConfig.cy, 0)
    assertEquals(finalConfig.rows.size, 1)
    assertEquals(finalConfig.rows(0).chars, Vector('a'.toByte, 'b'.toByte, 'd'.toByte))
  }