package domain
import scala.scalanative.unsafe.*
import `macro`.* 

inline val KILO_VERSION = "0.0.1"
inline val KILO_TAB_STOP = 2
inline val KILO_MSG = "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find"
inline val KILO_QUIT_TIMES = 3
inline val KILO_SYNTAX_DIR = "syntax"
inline val escInt = 0x1b
inline def ctrlKey(c: CChar): CChar = (c & 0x1f).toByte
inline val BACKSPACE = 127
val EXIT = ctrlKey('q')
val DELETE_BITS = ctrlKey('h')
val REFRESH_SCREEN = ctrlKey('l')
val SAVE = ctrlKey('s')
val FIND = ctrlKey('f')
val GOTO = ctrlKey('g')
inline val hideCursor = "[?25l"
inline val showCursor = "[?25h"
inline val clearScreen = "[2J"
inline val eraseInLine = "[K"
inline val resetCursor = "[H"
inline val resetFmt = "[0m"
inline val reverseVideo = "[7m"
inline val welcome = "Kilo editor -- version " + KILO_VERSION
inline def resetScreenCursorStr = escJoinStr(clearScreen, resetCursor)
def escJoinStrR(xs: String*): String = xs.toSeq.mkString(esc, esc, "")