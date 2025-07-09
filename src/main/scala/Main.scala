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
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*

object Main extends IOApp:
  def program[F[_]: MonadThrow: Defer: EditorConfigState](
      filenameOpt: Option[String],
      eventQueue: ConcurrentLinkedQueue[Event]
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
        Defer[F].defer(eventQueue.poll().pure).flatMap {
          case null => (Thread.sleep(1000.millis.toMillis).pure[F] >> loop)
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
    val eventQueue = new ConcurrentLinkedQueue[Event]()
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ =>
        Task.fork(EventLoop.create(eventQueue)) >>
          program[StateT[Task, EditorConfig, *]](args.headOption, eventQueue)
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
