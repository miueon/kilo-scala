package domain

import cats.MonadThrow
import cats.syntax.all.*
import os.Path
import java.nio.file.NoSuchFileException
import cats.data.Validated

enum HighlightType:
  def color: Int
  case Normal(color: Int = 39)
  case Number(color: Int = 31)
  case Match(color: Int = 46)
  case Str(color: Int = 32)
  case MLStr(color: Int = 132)
  case Comment(color: Int = 34)
  case MLComment(color: Int = 134)
  case Keyword1(color: Int = 33)
  case Keyword2(color: Int = 35)

  override def toString(): String =
    s"\u001b[${color}m"

case class SyntaxConfig(
    name: String = "",
    hightlightNumbers: Boolean = false,
    highlightSlStrs: Boolean = false,
    slCommentStart: Vector[String] = Vector.empty,
    mlCommentDelim: Option[(String, String)] = None,
    mlStrDelim: Option[String] = None,
    keywords: Vector[(HighlightType, Vector[String])] = Vector.empty
)

object SyntaxConfig:
  val wd = os.pwd
  def load[F[_]: MonadThrow](ext: String): F[Option[SyntaxConfig]] =
    def go(idx: Int, entries: IndexedSeq[Path]): F[Option[SyntaxConfig]] =
      if idx >= entries.size then none.pure
      else
        val entry = entries(idx)
        for
          lines <- os.read.lines(entry).pure[F]
          (syntaxConfig, exts) <- parseSyntaxConfig(lines)
          result <- if exts.contains(ext) then syntaxConfig.some.pure[F] else go(idx + 1, entries)
        yield result
    val readResult = for
      entries <- os.list(wd / KILO_SYNTAX_DIR).pure[F]
      syntaxConfigOpt <- go(0, entries)
    yield syntaxConfigOpt
    readResult.handleErrorWith(e =>
      if e.isInstanceOf[NoSuchFileException] then None.pure else MonadThrow[F].raiseError(e)
    )

  def parseSyntaxConfig[F[_]: MonadThrow](lines: IndexedSeq[String]): F[(SyntaxConfig, List[String])] =
    lines.toList.traverse(parseLine).flatMap { validatedConfigs =>
      val (errors, r) = validatedConfigs.separate
      if errors.nonEmpty then (new Exception(errors.mkString(", "))).raiseError
      else
        val (config, exts) = r.separate
        val mergedConfig = config.fold(SyntaxConfig())((acc, conf) =>
          acc.copy(
            name = if conf.name.nonEmpty then conf.name else acc.name,
            hightlightNumbers = conf.hightlightNumbers || acc.hightlightNumbers,
            highlightSlStrs = conf.highlightSlStrs || acc.highlightSlStrs,
            slCommentStart = conf.slCommentStart.appendedAll(acc.slCommentStart),
            mlCommentDelim = conf.mlCommentDelim.orElse(acc.mlCommentDelim),
            mlStrDelim = conf.mlStrDelim.orElse(acc.mlStrDelim),
            keywords = conf.keywords.appendedAll(acc.keywords)
          )
        )
        (mergedConfig, exts.flatten).pure
    }
  end parseSyntaxConfig

  def parseLine[F[_]: MonadThrow](line: String): F[Validated[String, Either[SyntaxConfig, List[String]]]] =
    val parts = line.split("=", 2)
    (parts.lift(0), parts.lift(1)) match
      case (Some(commentLine), _) if commentLine.startsWith("#") || commentLine.startsWith(";") =>
        SyntaxConfig().asLeft.valid.pure
      case (Some(k), Some(v))           => parseKeyValue(k, v)
      case (Some(""), None) | (None, _) => SyntaxConfig().asLeft.valid.pure
      case (Some(_), None)              => "Invalid line: $line".invalid.pure

  def parseKeyValue[F[_]: MonadThrow](
      key: String,
      value: String
  ): F[Validated[String, Either[SyntaxConfig, List[String]]]] =
    key match
      case "name"       => SyntaxConfig(name = value).asLeft.valid.pure
      case "extensions" => value.split(",").map(_.trim).toList.asRight.valid.pure
      case "highlight_numbers" =>
        parseBool(value).map(v => SyntaxConfig(hightlightNumbers = v).asLeft[List[String]].valid[String])
      case "highlight_strings" => parseBool(value).map(v => SyntaxConfig(highlightSlStrs = v).asLeft.valid)
      case "singleline_comment_start" =>
        SyntaxConfig(slCommentStart = value.split(",").map(_.trim).toVector).asLeft.valid.pure
      case "multiline_comment_delim" =>
        val delims = value.split(",").map(_.trim)
        if delims.length != 2 then "Expected 2 delimiters for multiline comment".invalid.pure
        else SyntaxConfig(mlCommentDelim = (delims(0), delims(1)).some).asLeft.valid.pure
      case "multiline_string_delim" => SyntaxConfig(mlStrDelim = value.some).asLeft.valid.pure
      case "keywords_1" =>
        SyntaxConfig(keywords =
          Vector((HighlightType.Keyword1(), value.split(",").map(_.trim).toVector))
        ).asLeft.valid.pure
      case "keywords_2" =>
        SyntaxConfig(keywords =
          Vector((HighlightType.Keyword2(), value.split(",").map(_.trim).toVector))
        ).asLeft.valid.pure
      case _ => s"Invalid key: $key".invalid.pure
  end parseKeyValue

  def parseBool[F[_]: MonadThrow](value: String): F[Boolean] =
    value.toLowerCase() match
      case "true" | "1" | "yes" => true.pure
      case "false" | "0" | "no" => false.pure
      case _                    => new IllegalArgumentException(s"Invalid boolean value: $value").raiseError
end SyntaxConfig
