package domain

import cats.syntax.all.*

enum HighlightType:
  def color: Int
  case Normal(color: Int = 39)
  case Number(color: Int = 31)
  case Match(color: Int = 46)
  case Str(color: Int = 32)
  case MLStr(color: Int = 132)
  case Comment(color: Int = 34)
  case MLComment(color: Int = 36)
  case Keyword1(color: Int = 33)
  case Keyword2(color: Int = 35)

  override def toString(): String =
    s"\u001b[${color}m"

case class SyntaxConfig(
    name: String = "",
    highlightNumbers: Boolean = false,
    highlightSlStrs: Boolean = false,
    slCommentStart: Vector[String] = Vector.empty,
    mlCommentDelim: Option[(String, String)] = None,
    mlStrDelim: Option[String] = None,
    keywords: Vector[(HighlightType, Vector[String])] = Vector.empty
)