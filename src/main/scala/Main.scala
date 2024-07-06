import cats.Defer
import cats.Eval
import cats.Monad
import cats.MonadThrow
import cats.data.EitherT
import cats.data.Kleisli
import cats.data.Reader
import cats.data.State
import cats.data.StateT
import cats.syntax.all.*
import effect.*
import effect.*
import effect.TaskEffect.*
import effect.TaskInterpretation.*
import effect.pull.Stream
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*
import rawmode.*
import rawmode.all.*
import rawmode.all.disableRawMode as resetRawMode
import rawmode.all.enableRawMode as setRawMode
import util.Utils.*

import java.nio.charset.Charset
import java.util.concurrent.Executors
import scala.scalanative.libc.{errno as libcErrno, *}
import scala.scalanative.posix.cpio
import scala.scalanative.posix.errno
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
import scala.util.Try

case class EditorConfig(a: Int)

type EditorConfigState[F[_]] = StateT[F, EditorConfig, Unit]
type EitherRawResult[A] = Either[Int, A]

type _errRaw[R] = EitherRawResult |= R

object Main extends IOApp:
  inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
  inline def resetCursorAndScreen[F[_]: MonadThrow: Defer](): F[Unit] =
    Defer[F].defer {
      unistd.write(unistd.STDOUT_FILENO, c"\x1b[2J", 4.toUInt).pure // reset screen
        >> unistd.write(unistd.STDOUT_FILENO, c"\x1b[H", 3.toUInt).pure // reset cursor, [x,xH
    }.void

  def editorReadKey[F[_]: MonadThrow: Defer](ref: Ref[F, Ptr[CChar]]): F[Unit] =
    for
      cPtr <- ref.get
      nread <- Defer[F].defer(unistd.read(unistd.STDIN_FILENO, cPtr, UInt.valueOf(1)).pure)
      _ <-
        if nread != 1 then editorReadKey(ref)
        else if nread == -1 && errno.errno != errno.EAGAIN then new Exception("read").raiseError
        else MonadThrow[F].unit
    yield ()

  def editorProcessKeypress[R: _task](): Eff[R, EitherRawResult[Unit]] =
    for
      cPtrRef <- fromTask(Ref[Task, Ptr[CChar]](malloc[CChar]).pure)
      _ <- fromTask(editorReadKey(cPtrRef))
      // r <- Task(println("keypressed")) >> Task(Left(0))
      cPtr <- fromTask(cPtrRef.get)
      // _ <- Task(println(s"keypressed: ${!cPtr}"))
      r <- !cPtr match
        case a if a == ctrlKey('q') => fromTask(resetCursorAndScreen[Task]() >> Task(Left(0)))
        case _                      => fromTask(Task(Right(())))
    yield r

  def editorDrawRows[F[_]: MonadThrow: Defer](): F[Unit] =
    Stream.eval(Defer[F].defer(unistd.write(unistd.STDOUT_FILENO, c"~\r\n", 3.toUInt).pure).void).repeat.take(24).run

  def editorRefreshScreen[R: _task](): Eff[R, Unit] =
    for
      _ <- fromTask(resetCursorAndScreen())
      _ <- fromTask(editorDrawRows())
      _ <- fromTask(Task(unistd.write(unistd.STDOUT_FILENO, c"\x1b[H", 3.toUInt)))
    yield ()

  type AppStack = Fx.fx1[Task]

  def program: Eff[AppStack, Unit] =
    def go(): Eff[AppStack, Unit] =
      for
        _ <- editorRefreshScreen[AppStack]()
        r <- editorProcessKeypress[AppStack]()
        _ <- if r.isLeft then Eff.pure(()) else go()
      yield ()
    go()

  def pureMain(args: List[String]): IO[Unit] =
    Resource
      .make[Task, TermIOS](TermIOS.enableRawMode)(TermIOS.disableRawMode)
      .use(_ => program.toTask)
      .handleErrorWith(e =>
        resetCursorAndScreen[Task]() >> Task.apply(
          Zone(stdio.printf(c"%s\n%s\n", toCString(e.getMessage), string.strerror(errno.errno)))
        )
      )
      .asIO
      .void
  end pureMain
end Main