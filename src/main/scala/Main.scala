import cats.Defer
import cats.MonadThrow
import cats.data.StateT
import cats.syntax.all.*
import cats.effect.*
import cats.effect.std.Queue
import cats.effect.std.Dispatcher
import domain.*
import effect.LiftIO
import rawmode.*
import domain.EditorConfigState
import services.EditorOps
import services.SyntaxConfigOps
import services.KeyOps
import par.Event
import par.EventLoop

object Main extends IOApp:
  def program[F[_]: MonadThrow: Defer: EditorConfigState: LiftIO](
      filenameOpt: Option[String],
      eventQueue: Queue[IO, Event]
  ): F[Unit] =
    val syntaxOps = SyntaxConfigOps.make[F]
    val keyOps = KeyOps.make[F]
    val editorOps = EditorOps.make[F](syntaxOps, keyOps)
    def loop: F[Unit] =
      val updateTask = for
        _ <- editorOps.scroll
        config <- EditorConfigState[F].get
        _ <- config.promptMode.fold(().pure[F])(p => editorOps.updateStatusMsg(p.statusMsg.some))
        _ <- editorOps.refreshScreen(config)
      yield ()

      updateTask.flatMap { _ =>
        LiftIO[F].lift(eventQueue.take).flatMap {
          case Event.Key(k: domain.Key) =>
            EditorConfigState[F].get.flatMap { config =>
              config.promptMode
                .fold(editorOps.processKeypress(k))(p => editorOps.processPromptKeypress(p, k))
                .flatMap {
                  case Left(v)   => MonadThrow[F].raiseError(new Exception(s"Exit code: $v"))
                  case Right(()) => loop
                }
            }
          case Event.WindowResize =>
            editorOps.updateWindowsSize.flatMap(_ => loop)
          case Event.Quit =>
            MonadThrow[F].raiseError(new Exception("Exit"))
        }
      }
    end loop

    for
      _ <- editorOps.updateWindowsSize
      _ <- editorOps.openFile(filenameOpt)
      _ <- loop
    yield ()
  end program

  def run(args: List[String]): IO[ExitCode] =
    (for
      eventQueue <- Resource.eval[IO, Queue[IO, Event]](
        Queue.unbounded[IO, Event]
      )
      dispatcher <- Dispatcher.parallel[IO]
      _ <- Resource.make[IO, Unit](
        EventLoop.create(eventQueue, dispatcher).start.void
      )(_ => IO.unit)
      _ <- Resource.make[IO, TermIOS](TermIOS.enableRawMode[IO])(
        TermIOS.disableRawMode[IO]
      )
      res <- Resource.eval[IO, (EditorConfig, Unit)](
        program[StateT[IO, EditorConfig, *]](
          args.headOption,
          eventQueue
        ).run(
          EditorConfig(
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            KILO_QUIT_TIMES,
            false,
            StatusMessage(KILO_MSG).some
          )
        )
      )
    yield res)
      .use(_ => IO.unit)
      .handleErrorWith(e =>
        EditorOps.resetScreenCursor[IO] >>
          IO(
            printf(f"%%s\n", e.getMessage())
          )
      )
      .as(ExitCode.Success)
end Main
