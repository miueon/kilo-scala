import `macro`.*
import cats.Defer
import cats.MonadThrow
import cats.data.StateT
import cats.syntax.all.*
import domain.*
import effect.*
import rawmode.*
import services.EditorConfigState
import services.EditorOps
import services.SyntaxConfigOps

object Main extends IOApp:
  def program[F[_]: MonadThrow: Defer: EditorConfigState](filenameOpt: Option[String]): F[Unit] =
    val syntaxOps = SyntaxConfigOps.make
    val editorOps = EditorOps.make(syntaxOps)
    def go: F[Unit] =
      for
        _ <- editorOps.updateWindowsSize
        _ <- editorOps.scroll
        config <- EditorConfigState[F].get
        _ <- config.promptMode.fold(().pure)(p => editorOps.updateStatusMsg(p.statusMsg.some))
        config <- EditorConfigState[F].get
        _ <- editorOps.refreshScreen(config)
        k <- editorOps.readKey
        r <- config.promptMode.fold(editorOps.processKeypress(k))(p => editorOps.processPromptKeypress(p, k))
        _ <- r match
          case Left(v)   => MonadThrow[F].raiseError(new Exception(s"Exit code: $v"))
          case Right(()) => go
      yield ()
    for
      _ <- editorOps.updateWindowsSize
      _ <- editorOps.openFile(filenameOpt)
      _ <- go
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        program[StateT[Task, EditorConfig, *]](args.headOption)
          .run(EditorConfig(0, 0, 0, 0, 0, 0, 0, KILO_QUIT_TIMES, false, StatusMessage(KILO_MSG).some))
          .map(_._2)
      )
      .handleErrorWith(e =>
        EditorOps.resetScreenCursor[Task] >>
          Task.apply(
            printf(f"%%s\n", e.getMessage())
          )
      )
      .asIO
      .void
end Main
