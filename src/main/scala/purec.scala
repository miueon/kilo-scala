package purec
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.UInt
import scala.scalanative.runtime.ffi
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

import TerminOSOps.*

object RawConsoleInput:
  private val STDIN_FILENO = unistd.STDERR_FILENO
  private var originalTermios = stackalloc[termios.termios]()
  private var rawTermios = stackalloc[termios.termios]()
  private var isRawMode = false

  def enableRawMode() =
    termios.tcgetattr(unistd.STDIN_FILENO, originalTermios)
    ffi.memcpy(rawTermios, originalTermios, sizeof[termios.termios])

    rawTermios.c_lflag = 0x8a3b & (~termios.ECHO)
    isRawMode = true
    stdio.printf(c"0x%08x\n", rawTermios.c_lflag)
    termios.tcsetattr(unistd.STDIN_FILENO, termios.TCSAFLUSH, rawTermios)

  def readChar(wait: Boolean): CChar =
    if !isRawMode then enableRawMode()
    val buf = stackalloc[Byte]()
    val readResult = unistd.read(STDIN_FILENO, buf, UInt.valueOf(1))
    !buf
end RawConsoleInput

// def enableRaw =
//   val orig_termios: Ptr[termios.termios] = stackalloc[termios.termios]()
//   termios.tcgetattr(0, orig_termios)
//   stdio.printf(c"ciflag: 0x%08x\n", orig_termios.c_iflag)
//   stdio.printf(c"coflag: 0x%08x\n", orig_termios.c_oflag)
//   stdio.printf(c"clflag: 0x%08x\n", orig_termios.c_lflag)
// //   term.c_lflag = 0;
// // term.c_lflag |= ECHO | ICANON | ISIG | IEXTEN;
//   orig_termios.c_lflag = ~(termios.ECHO | termios.ICANON | termios.ISIG | termios.ECHONL)
//   stdio.printf(c"clflag: 0x%08x\n", orig_termios.c_lflag)
//   val result = orig_termios.c_lflag & ~termios.ECHO
//   orig_termios.c_lflag = result
//   stdio.printf(c"0x%08x\n", result)
//   termios.tcsetattr(0, termios.TCSANOW, orig_termios)
//   println(orig_termios.c_lflag)

// @extern
// object testraw:
//   def enableRawMode(): Unit = extern
// import testraw.all.*
// @main
// def test =
//   enableRawMode()

//   // println(unistd.isatty(0))
//   // enableRaw

//   // var c = stackalloc[CChar]()
//   // while true do
//   //   stdio.scanf(c"%c", c)

//   // var c = stackalloc[CChar]()
//   // !c = RawConsoleInput.readChar(true)
//   var c = stackalloc[CChar]()

//   while unistd.read(unistd.STDIN_FILENO, c, UInt.valueOf(1)) == 1 && !c != 'q' do println((!c).toChar)
  // //   !c = RawConsoleInput.readChar(true)
