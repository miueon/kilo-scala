package rawmode
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.USize
import scala.scalanative.unsigned.UInt
import cats.MonadThrow
import rawmode.all.{disableRawMode as resetRawMode, enableRawMode as setRawMode}
import cats.Monad
import effect.Task
import cats.Eval

object TerminOSOps:
  extension (t: Ptr[termios.termios])
    def c_iflag: termios.tcflag_t = t._1
    def c_oflag: termios.tcflag_t = t._2
    def c_cflag: termios.tcflag_t = t._3
    def c_lflag: termios.tcflag_t = t._4
    def c_lflag_=(v: termios.tcflag_t): Unit = t._4 = v
    def c_cc: termios.c_cc = t._5
    def c_ispeed: termios.speed_t = t._6
    def c_ospeed: termios.speed_t = t._7

case class TermIOS(orig: Ptr[termios.termios])

object TermIOS:
  def enableRawMode: Task[TermIOS] =
    Task {
      val orig = malloc[termios.termios]
      setRawMode(orig)
      TermIOS(orig)
    }

  def disableRawMode(t: TermIOS): Task[Unit] =
    Task {
      resetRawMode(t.orig)
      stdlib.free(t.orig)
      println("freed")
    }
end TermIOS
