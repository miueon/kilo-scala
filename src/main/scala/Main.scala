import `macro`.*
import cats.Defer
import cats.MonadThrow
import cats.data.StateT
import cats.syntax.all.*
import domain.*
import editor.*
import editor.Editor.*
import effect.*
import rawmode.*

object Main extends IOApp:
  def program[F[_]: MonadThrow: Defer](filenameOpt: Option[String]): EditorConfigState[F, Unit] =
    def go: EditorConfigState[F, Unit] =
      for
        _ <- editorScroll
        config <- StateT.get[F, EditorConfig]
        _ <- config.promptMode.fold(().pure[EditorConfigState[F, *]])(p => updateStatusMsg(p.statusMsg.some))
        _ <- StateT.inspectF(editorRefreshScreen(_))
        k <- StateT.liftF(editorReadKey())
        r <- config.promptMode.fold(editorProcessKeypress(k))(_.processKeypress(k))
        _ <- r match
          case Left(v)   => StateT.liftF(MonadThrow[F].raiseError(new Exception(s"Exit code: $v")))
          case Right(()) => go
      yield ()
    for
      _ <- initEditor
      _ <- editorOpen(filenameOpt)
      _ <- go
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        program[Task](args.headOption)
          .run(EditorConfig(0, 0, 0, 0, 0, 0, 0, KILO_QUIT_TIMES, false, StatusMessage(KILO_MSG).some))
          .map(_._2)
      )
      .handleErrorWith(e =>
        resetScreenCursor[Task] >>
          Task.apply(
            printf(f"%%s\n", e.getMessage())
          )
      )
      .asIO
      .void
end Main
