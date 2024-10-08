package rawmode
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.unsafe.*
import rawmode.all.{disableRawMode as resetRawMode, enableRawMode as setRawMode}
import cats.Monad
import util.Utils.*
import cats.Defer

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
  def enableRawMode[F[_]: Monad]: F[TermIOS] =
    Monad[F].pure {
      val orig = malloc[termios.termios]
      if setRawMode(orig) < 0 then throw new Exception("enableRawMode failed")
      else TermIOS(orig)
    }

  def disableRawMode[F[_]: Monad: Defer](t: TermIOS): F[Unit] =
    Defer[F].defer {
      Monad[F].pure {
        val result = resetRawMode(t.orig)
        stdlib.free(t.orig)
        if result < 0 then throw new Exception("disableRawMode failed")
        else println("freed")
      }
    }
end TermIOS
