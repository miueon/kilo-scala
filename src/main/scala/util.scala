package util

import scala.scalanative.unsigned.UInt
import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

object Utils:
  inline def malloc[A]: Ptr[A] = stdlib.malloc(sizeof[A]).asInstanceOf[Ptr[A]]

  extension (i: Int) def toUInt = UInt.valueOf(i)

