package util

import scala.scalanative.unsigned.UInt
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

object Utils:
  inline def malloc[A]: Ptr[A] = stdlib.malloc(sizeof[A]).asInstanceOf[Ptr[A]]

  extension (s: String) def hexToChar = Integer.parseInt(s, 16).toChar

