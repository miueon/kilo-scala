package `macro`

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Varargs

def escImpl(delim: Expr[String], xs: Expr[Seq[String]])(using Quotes) =
  val d = delim.valueOrAbort
  val strs = xs match
    case Varargs(ys) => ys.map(_.valueOrAbort)
    case _           => xs.valueOrAbort

  val result = strs.mkString(d, d, "")
  Expr(result)

def prependEsc(s: Expr[String])(using Quotes): Expr[String] =
  '{
    esc + $s
  }

inline val esc = "\u001b"

inline def escJoinStr(inline xs: String*): String = ${ escImpl('esc, 'xs) }

extension (inline s: String)
  inline def esc: String =
    ${ prependEsc('s) }
