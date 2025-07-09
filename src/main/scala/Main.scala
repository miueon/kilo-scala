import `macro`.*
import cats.Defer
import cats.MonadThrow
import cats.data.StateT
import cats.syntax.all.*
import domain.*
import effect.*
import rawmode.*
import domain.EditorConfigState
import services.EditorOps
import services.SyntaxConfigOps
import services.KeyOps
import par.Event
import par.EventLoop
import scala.concurrent.duration.*
import effect.UnsafeQueue

object Main extends IOApp:
  def program[F[_]: MonadThrow: Defer: EditorConfigState: LiftTask](
      filenameOpt: Option[String],
      eventQueue: UnsafeQueue[Event]
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
        LiftTask[F].lift(eventQueue.take).flatMap {
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

    for
      _ <- editorOps.updateWindowsSize
      _ <- editorOps.openFile(filenameOpt)
      _ <- loop
    yield ()
  end program

  def pureMain(args: List[String]): IO[Unit] =
    (for
      eventQueue <- Resource.eval[Task, UnsafeQueue[Event]](
        UnsafeQueue.unbounded[Event]
      )
      _ <- Resource.make[Task, Unit](
        Task.fork(EventLoop.create(eventQueue))
      )(_ => Task.unit)
      given MonadThrow[Task] = Task.monad
      _ <- Resource.make[Task, TermIOS](TermIOS.enableRawMode[Task])(
        TermIOS.disableRawMode[Task]
      )
      res <- Resource.eval[Task, (EditorConfig, Unit)](
        program[StateT[Task, EditorConfig, *]](
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
    yield res).use(_ => Task.unit)
      .handleErrorWith(e =>
        EditorOps.resetScreenCursor[Task] >>
          Task.apply(
            printf(f"%%s\n", e.getMessage())
          )
      )
      .asIO
      .void
end Main
