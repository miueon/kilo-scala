import cats.data.StateT
import cats.mtl.Stateful
import domain.EditorConfig
import domain.SyntaxConfig
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import services.KeyOps
import cats.syntax.all.*

import domain.EditorConfigState
import domain.AKey

trait KeyOpsTestUtils:
  def defaultEditorConfig: EditorConfig

  def runKeyOps[A](initialConfig: EditorConfig = defaultEditorConfig)(
      op: KeyOps[StateT[IO, EditorConfig, *]] => StateT[IO, EditorConfig, A]
  ): (EditorConfig, A) =
    val keyOps = KeyOps.make[StateT[IO, EditorConfig, *]]
    val program = for result <- op(keyOps) yield result
    program.run(initialConfig).unsafeRunSync()

class KeyOpsTest extends munit.FunSuite with KeyOpsTestUtils:
  val defaultEditorConfig = EditorConfig(
    cx = 0,
    cy = 0,
    rx = 0,
    rowoff = 0,
    coloff = 0,
    screenRows = 24,
    screenCols = 80,
    quitTimes = 3,
    dirty = false,
    statusMsg = None,
    syntax = SyntaxConfig(),
    promptMode = None,
    rows = Vector.empty,
    filename = None
  )
  

  test("insertChar") {
    val (finalConfig, _) = runKeyOps[Unit](defaultEditorConfig) { (keyOps: KeyOps[StateT[IO, EditorConfig, *]]) =>
      for
        _ <- keyOps.insertChar('X')
        _ <- keyOps.insertChar('Y')
        _ <- keyOps.insertChar('Z')
      yield ()
    }

    assertEquals(finalConfig.cx, defaultEditorConfig.cx + 3)
    assertEquals(finalConfig.rows.length, 1)
    assertEquals(finalConfig.rows(0).chars, Vector('X', 'Y', 'Z').map(_.toByte))
  }

  test("insertNewline") {
    val (finalConfig, _) = runKeyOps[Unit](defaultEditorConfig) { (keyOps: KeyOps[StateT[IO, EditorConfig, *]]) =>
      keyOps.insertNewLine.replicateA(3).void
    }

    assertEquals(finalConfig.cy, defaultEditorConfig.cy + 3)
    assertEquals(finalConfig.rows.length, 3)
    finalConfig.rows.foreach(row => assertEquals(row.chars, Vector.empty))
  }

  test("deleteChar") {
    val (finalConfig, _) = runKeyOps[Unit](defaultEditorConfig) { (keyOps: KeyOps[StateT[IO, EditorConfig, *]]) =>
      for
        _ <- "Hello world!".map(_.toByte).map(keyOps.insertChar).reduce((a, b) => a >> b)
        _ <- keyOps.deleteChar
        deleteCharConfig <- EditorConfigState[StateT[IO, EditorConfig, *]].get
        _ <- keyOps.moveCursor(AKey.Left)
        _ <- keyOps.moveCursor(AKey.Left)
        _ <- keyOps.moveCursor(AKey.Left)
        _ <- keyOps.deleteChar
      yield
        assertEquals(deleteCharConfig.rows(0).chars, "Hello world".map(_.toByte))
        ()
    }

    assertEquals(finalConfig.rows(0).chars, "Hello wrld".map(_.toByte))
    assertEquals(finalConfig.cx, 7)
  }
end KeyOpsTest
