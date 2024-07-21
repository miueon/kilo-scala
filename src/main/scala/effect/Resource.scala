package effect
import cats.MonadThrow
import cats.syntax.all.*
import effect.pull.*
import cats.Monad
import cats.MonadError

abstract class Resource[F[_], A]:
  def use[B](f: A => F[B]): F[B]

object Resource:
  def make[F[_]: MonadThrow, A](acquire: F[A])(release: A => F[Unit]): Resource[F, A] =
    new Resource[F, A]:
      def use[B](f: A => F[B]): F[B] =
        Stream.resource(acquire)(release).mapEval(f).toList.map(_.headOption.get)
