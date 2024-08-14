package services

import cats.MonadThrow
import cats.data.Validated
import cats.syntax.all.*
import domain.HighlightType
import domain.KILO_SYNTAX_DIR
import domain.SyntaxConfig
import os.Path

import java.nio.file.NoSuchFileException

trait SyntaxConfigOps[F[_]]:
  def load(ext: String): F[Option[SyntaxConfig]]

object SyntaxConfigOps:
  private val wd = os.pwd
  def make[F[_]: MonadThrow]: SyntaxConfigOps[F] =
    new:
      def load(ext: String): F[Option[SyntaxConfig]] = 
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

      private def parseSyntaxConfig(lines: IndexedSeq[String]): F[(SyntaxConfig, List[String])] =
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

      private def parseLine(line: String): F[Validated[String, Either[SyntaxConfig, List[String]]]] =
        val parts = line.split("=", 2)
        (parts.lift(0), parts.lift(1)) match
          case (Some(commentLine), _) if commentLine.startsWith("#") || commentLine.startsWith(";") =>
            SyntaxConfig().asLeft.valid.pure
          case (Some(k), Some(v))           => parseKeyValue(k, v)
          case (Some(""), None) | (None, _) => SyntaxConfig().asLeft.valid.pure
          case (Some(_), None)              => "Invalid line: $line".invalid.pure

      private def parseKeyValue(
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

      private def parseBool(value: String): F[Boolean] =
        value.toLowerCase() match
          case "true" | "1" | "yes" => true.pure
          case "false" | "0" | "no" => false.pure
          case _                    => new IllegalArgumentException(s"Invalid boolean value: $value").raiseError

