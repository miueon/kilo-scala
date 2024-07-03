package rawmode
import scala.scalanative.libc.*
import scala.scalanative.posix.termios
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*

inline def malloc[A]: Ptr[A] = stdlib.malloc(sizeof[A]).asInstanceOf[Ptr[A]]
