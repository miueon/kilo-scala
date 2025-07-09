package effect
import cats.{Monad, MonadThrow}
import cats.syntax.all.*
import effect.pull.*

abstract class Resource[F[_], A]:
  self =>
  def use[B](f: A => F[B]): F[B]
  def flatMap[B](f: A => Resource[F, B])(using Monad[F]): Resource[F, B] =
    new Resource[F, B]:
      def use[C](g: B => F[C]): F[C] =
        self.use(a => f(a).use(g))
  def map[B](f: A => B)(using MonadThrow[F]): Resource[F, B] =
    flatMap(a => Resource.eval(f(a).pure[F]))

object Resource:
  given [F[_]: MonadThrow]: Monad[Resource[F, *]] with
    def pure[A](x: A): Resource[F, A] = Resource.eval(x.pure[F])
    def flatMap[A, B](fa: Resource[F, A])(f: A => Resource[F, B]): Resource[F, B] =
      fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Resource[F, Either[A, B]]): Resource[F, B] =
      new Resource[F, B]:
        def use[C](g: B => F[C]): F[C] =
          f(a).use {
            case Left(a)  => tailRecM(a)(f).use(g)
            case Right(b) => g(b)
          }

  def make[F[_]: MonadThrow, A](acquire: F[A])(release: A => F[Unit]): Resource[F, A] =
    new Resource[F, A]:
      def use[B](f: A => F[B]): F[B] =
        Stream.resource(acquire)(release).mapEval(f).toList.map(_.headOption.get)

  def eval[F[_]: MonadThrow, A](fa: F[A]): Resource[F, A] =
    make(fa)(_ => ().pure[F])
