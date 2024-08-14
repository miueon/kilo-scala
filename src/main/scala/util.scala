package util

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

object Utils:
  inline def malloc[A]: Ptr[A] = stdlib.malloc(sizeof[A]).asInstanceOf[Ptr[A]]

  extension (s: String) 
    def hexToChar = Integer.parseInt(s, 16).toChar
    def removeSuffixNewLine: String = s.stripSuffix("\n").stripSuffix("\r")

  extension (i: Int)
    infix def saturatingSub(j: Int) = if i < j then 0 else i - j

