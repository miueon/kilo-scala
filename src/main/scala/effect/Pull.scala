package effect

import cats.Monad
import cats.kernel.Monoid
import cats.syntax.all.*
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls
import cats.MonadThrow
import scala.util.Try

enum StepResult[F[_], +O, +R]:
  case Done(scope: Scope[F], result: R)
  case Out(scope: Scope[F], head: O, tail: Pull[F, O, R])
  def toUnconsResult: Either[R, (O, Pull[F, O, R])] = this match
    case Done(_, result)    => Left(result)
    case Out(_, head, tail) => Right((head, tail))

enum Pull[+F[_], +O, +R]:
  case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
  case Output[+O](value: O) extends Pull[Nothing, O, Unit]
  case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
  case Uncons(source: Pull[F, O, R]) extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]
  case FlatMap[+F[_], X, +O, +R](
      source: Pull[F, O, X],
      f: X => Pull[F, O, R]
  ) extends Pull[F, O, R]
  case FlatMapOutput[+F[_], O, +O2](source: Pull[F, O, Unit], f: O => Pull[F, O2, Unit]) extends Pull[F, O2, Unit]
  case Handle[+F[_], +O, +R](
      source: Pull[F, O, R],
      handler: Throwable => Pull[F, O, R]
  ) extends Pull[F, O, R]
  case Error(t: Throwable) extends Pull[Nothing, Nothing, Nothing]
  case OpenScope(source: Pull[F, O, R], finalizer: Option[F[Unit]]) extends Pull[F, O, R]
  case WithScope(source: Pull[F, O, R], scopeId: Id, returnScope: Id) extends Pull[F, O, R]

  def step[F2[x] >: F[x]: MonadThrow, O2 >: O, R2 >: R](scope: Scope[F2]): F2[StepResult[F2, O2, R2]] =
    import StepResult.{Done, Out}
    this match
      case Result(result) => Done(scope, result).pure
      case Output(value) =>
        Out(scope, value, Pull.done).pure
      case Eval(action)   => action.asInstanceOf[F2[R]].map(Done(scope, _))
      case Uncons(source) => source.step(scope).map(s => Done(scope, s.toUnconsResult.asInstanceOf[R2]))
      case FlatMap(source, f) =>
        source match
          // FlatMap(FlatMap(s2, g), f) => FlatMap(s2, FlatMap(g(s2=>x), f) )
          case FlatMap(s2, g) =>
            s2.flatMap(x => g(x).flatMap(f)).asInstanceOf[Pull[F2, O, R]].step(scope)
          case other =>
            other.step(scope).flatMap {
              case Done(scope, r) => f(r).step(scope)
              case Out(scope, hd, tl) =>
                Out(scope, hd, tl.flatMap(f)).pure
            }
      case FlatMapOutput(source, f) =>
        source.step(scope).flatMap {
          case Done(scope, r)         => Done(scope, r).pure
          case Out(scope, head, tail) => f(head).flatMap(_ => tail.flatMapOutput(f)).step(scope)
        }

      // reason why left nested FlatMaps will lead to stack overflow
      // is caused by scala default execution model is strict
      // So when it encounter with a flatMap call will gen a new stack frame
      // if the flatMap layer is too large then it is possibly lead to stack overflow
      // But if we put rewrites it with right nested, then it is wrapped under a new func
      case Handle(source, f) =>
        source match
          case Handle(s2, g) =>
            s2.handleErrorWith(x => g(x).handleErrorWith(y => f(y)))
              .step(scope) // similar to flatMap(FlatMap, f) we need to do the trampoline first
          // So actually the Handle is quite similar to FlatMap, but one is for error channel one is for happy path
          // and ultimatly, we deletegate the error handling to F2 after all
          case other =>
            other
              .step(scope)
              .map {
                case Out(scope, hd, tl) => Out(scope, hd, Handle(tl, f))
                case Done(scope, r)     => Done(scope, r) // propogate the handle to inner pull
              }
              .handleErrorWith(t => f(t).step(scope))
      case Error(t) => t.raiseError
      // TODO
      case OpenScope(source, finalizer) =>
        scope
          .open(finalizer.getOrElse(().pure))
          .flatMap(subscope => WithScope(source, subscope.id, scope.id).step(subscope))
      case WithScope(source, scopeId, returnScopeId) =>
        scope.findScope(scopeId).map(_.map(_ -> true).getOrElse(scope -> false)).flatMap {
          case (newScope, closeAfterUse) =>
            source.step(newScope).attempt.flatMap {
              case Right(Out(scope, hd, tl)) => Out(scope, hd, WithScope(tl, scopeId, returnScopeId)).pure
              case Right(Done(outScope, r)) =>
                scope
                  .findScope(returnScopeId)
                  .map(_.getOrElse(outScope))
                  .flatMap(nextScope => scope.close.as(Done(nextScope, r)))
              case Left(t) => scope.close.flatMap(_ => t.raiseError)
            }
        }

    end match
  end step

  final def fold[F2[x] >: F[x]: MonadThrow, R2 >: R, A](
      init: A
  )(f: (A, O) => A): F2[(R2, A)] =
    val scope = Scope.root[F2]
    def go(scope: Scope[F2], p: Pull[F2, O, R2], acc: A): F2[(R2, A)] =
      p.step(scope).flatMap {
        case StepResult.Done(_, result)           => (result, init).pure
        case StepResult.Out(newScope, head, tail) => go(newScope, tail, f(init, head))
      }
    go(scope, this, init).attempt.flatMap(res => scope.close.flatMap(_ => res.fold(_.raiseError, _.pure)))
    // step.flatMap {
    //   case Left(value) => (value, init).pure
    //   case Right((hd, tl)) =>
    //     tl.fold(f(init, hd))(f)
    // }

  def toList[F2[x] >: F[x]: MonadThrow, O2 >: O]: F2[List[O2]] =
    fold(List.newBuilder[O])((bldr, o) => bldr += o).map(_(1).result())

  def flatMap[F2[x] >: F[x], O2 >: O, R2](
      f: R => Pull[F2, O2, R2]
  ): Pull[F2, O2, R2] =
    Pull.FlatMap(this, f)

  def >>[F2[x] >: F[x], O2 >: O, R2](
      next: => Pull[F2, O2, R2]
  ): Pull[F2, O2, R2] =
    flatMap(_ => next)

  def handleErrorWith[F2[x] >: F[x], O2 >: O, R2 >: R](
      handler: Throwable => Pull[F2, O2, R2]
  ): Pull[F2, O2, R2] =
    Pull.Handle(this, handler)

  def map[R2](f: R => R2): Pull[F, O, R2] =
    flatMap(r => Result(f(r)))

  def repeat: Pull[F, O, Nothing] =
    this >> repeat

  def uncons: Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]] =
    Uncons(this)

  def take(n: Int): Pull[F, O, Option[R]] =
    if n <= 0 then Result(None)
    else
      // step match
      //   case Left(r)         => Result(Some(r))
      //   case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
      uncons
        .flatMap {
          case Left(r)         => Result(Some(r))
          case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
        }
      // Wrap with a layer of Result + FlatMap, so the take won't eagerly pull the value out
      // but like other combinator that delay to the fold

  def drop(n: Int): Pull[F, O, R] =
    if n <= 0 then this
    else
      uncons
        .flatMap {
          case Left(r)         => Result(r)
          case Right((hd, tl)) => tl.drop(n - 1)
        }

  def takeWhile(p: O => Boolean): Pull[F, O, Pull[F, O, R]] =
    uncons
      .flatMap {
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if p(hd) then Output(hd) >> tl.takeWhile(p)
          else Result(Output(hd) >> tl)
      }

  def dropWhil(p: O => Boolean): Pull[F, Nothing, Pull[F, O, R]] =
    uncons
      .flatMap {
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if p(hd) then tl.dropWhil(p)
          else Result(Output(hd) >> tl)
      }

  def mapOutput[O2](f: O => O2): Pull[F, O2, R] =
    uncons.flatMap {
      case Left(r)         => Result(r)
      case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
    }

  def filter(p: O => Boolean): Pull[F, O, R] =
    uncons
      .flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) =>
          if p(hd) then Output(hd) >> tl.filter(p)
          else tl.filter(p)
      }

  def count: Pull[F, Int, R] =
    def go(total: Int, p: Pull[F, O, R]): Pull[F, Int, R] =
      p.uncons.flatMap {
        case Left(r) => Result(r)
        case Right((_, tl)) =>
          val newTotal = total + 1
          Output(newTotal) >> go(newTotal, tl)
      }
    Output(0) >> go(0, this)

  def countViaAcc: Pull[F, Int, R] =
    Output(0) >>
      mapAccumulate(0) { (total, _) =>
        val newTotal = total + 1
        (newTotal, newTotal)
      }
        .map(_._2)

  def tally[O2 >: O: Monoid]: Pull[F, O2, R] =
    def go(total: O2, p: Pull[F, O, R]): Pull[F, O2, R] =
      p.uncons
        .flatMap {
          case Left(r) => Result(r)
          case Right((hd, tl)) =>
            val newTotal = Monoid.combine(total, hd)
            Output(newTotal) >> go(newTotal, tl)
        }
    Output(Monoid.empty) >> go(Monoid.empty, this)

  def tallyViaAccp[O2 >: O: Monoid]: Pull[F, O2, R] =
    mapAccumulate(Monoid.empty)((total, o) => (Monoid.combine(total, o), Monoid.combine(total, o))).map(_._2)

  def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[F, O2, (S, R)] =
    uncons
      .flatMap {
        case Left(r) =>
          Result((init, r))
        case Right((hd, tl)) =>
          val (s, out) = f(init, hd)
          Output(out) >> tl.mapAccumulate(s)(f)
      }
end Pull

object Pull:
  val done: Pull[Nothing, Nothing, Unit] = Result(())

  def fromList[O](os: List[O]): Pull[Nothing1, O, Unit] =
    os match
      case Nil          => done
      case head :: next => Output(head) >> fromList(next)

  def fromLazyList[O](os: LazyList[O]): Pull[Nothing1, O, Unit] =
    os match
      case LazyList()    => done
      case head #:: next => Output(head) >> fromLazyList(next)

  def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[Nothing1, O, R] =
    f(init) match
      case Left(r)        => Result(r)
      case Right((o, r2)) => Output(o) >> unfold(r2)(f)

  def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[Nothing1, O, Unit] =
    unfold(os) {
      case LazyList() => Left(LazyList())
      case hd #:: tl  => Right((hd, tl))
    }.map(_ => ())

  def continually[A](a: A): Pull[Nothing1, A, Nothing] =
    Output(a).repeat

  def iterate[O](init: O)(f: O => O): Pull[Nothing1, O, Nothing] =
    Output(init) >> iterate(f(init))(f)

  given [F[_], O]: Monad[[x] =>> Pull[F, O, x]] with
    def pure[A](x: A): Pull[F, O, A] = Result(x)
    def flatMap[A, B](fa: Pull[F, O, A])(f: A => Pull[F, O, B]): Pull[F, O, B] =
      fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Pull[F, O, Either[A, B]]): Pull[F, O, B] =
      f(a)
        .flatMap {
          case Left(o)  => tailRecM(o)(f)
          case Right(r) => Result(r)
        }

  extension [R](self: Pull[Nothing1, Int, R])
    def slidingMean(n: Int): Pull[Nothing1, Double, R] =
      def go(
          window: collection.immutable.Queue[Int],
          p: Pull[Nothing1, Int, R]
      ): Pull[Nothing1, Double, R] =
        p.uncons.flatMap {
          case Left(r) => Result(r)
          case Right((hd, tl)) =>
            val newWindow =
              if window.size < n then window :+ hd else window.tail :+ hd
            val meanOfNewWindow = newWindow.sum / newWindow.size.toDouble
            Output(meanOfNewWindow) >> go(newWindow, tl)
        }
      go(collection.immutable.Queue.empty, self)

    def slidingMeanViaAcc(n: Int): Pull[Nothing1, Double, R] =
      self
        .mapAccumulate(collection.immutable.Queue.empty[Int]) { (window, hd) =>
          val newWindow =
            if window.size < n then window :+ hd else window.tail :+ hd
          val meanOfNewWindow = newWindow.sum / newWindow.size.toDouble
          (newWindow, meanOfNewWindow)
        }
        .map(_._2)
  end extension

  extension [F[_], O](self: Pull[F, O, Unit])
    def flatMapOutput[O2](f: O => Pull[F, O2, Unit]): Pull[F, O2, Unit] =
      // self.uncons
      //   .flatMap {
      //     case Left(()) => Result(())
      //     case Right((hd, tl)) =>
      //       f(hd) >> (tl.flatMapOutput(f))
      //   }
      FlatMapOutput(self, f)

    def toStream: Stream[F, O] = Stream.fromPull(self)
end Pull

opaque type Stream[+F[_], +O] = Pull[F, O, Unit]

object Stream:
  def empty: Stream[Nothing1, Nothing] = Pull.done

  def apply[O](os: O*): Stream[Nothing1, O] =
    Pull.fromList(os.toList)

  def fromList[O](os: List[O]): Stream[Nothing1, O] = Pull.fromList(os)

  def fromLazyList[O](os: LazyList[O]): Stream[Nothing1, O] =
    Pull.fromLazyList(os)

  def unfold[O, R](init: R)(f: R => Option[(O, R)]): Stream[Nothing1, O] =
    Pull.unfold(init)(r => f(r).toRight(r)).void

  def continually[O](o: O): Stream[Nothing1, O] =
    Pull.Output(o) >> continually(o)

  def iterate[O](init: O)(f: O => O): Stream[Nothing1, O] =
    Pull.iterate(init)(f)

  def eval[F[_], O](fo: F[O]): Stream[F, O] =
    Pull.Eval(fo).flatMap(Pull.Output(_))
  // Pull.Eval will put the O to R, flatMap(Output(_)) here will convert R to O

  def unfoldEval[F[_], O, R](init: R)(f: R => F[Option[(O, R)]]): Stream[F, O] =
    Pull
      .Eval(f(init))
      .flatMap {
        case None         => Stream.empty
        case Some((o, r)) => Pull.Output(o) ++ unfoldEval(r)(f)
      }

  def fromPull[F[_], O](p: Pull[F, O, Unit]): Stream[F, O] = p

  def raiseError[F[_], O](t: Throwable): Stream[F, O] = Pull.Error(t)

  def resource[F[_], R](acquire: F[R])(release: R => F[Unit]): Stream[F, R] =
    Pull.Eval(acquire).flatMap(r => Pull.OpenScope(Pull.Output(r), Some(release(r))))

  extension [F[_], O](self: Stream[F, O])
    def toPull: Pull[F, O, Unit] = self

    def fold[A](init: A)(f: (A, O) => A)(using MonadThrow[F]): F[A] =
      self.fold[F, Unit, A](init)(f).map(_._2)

    def toList(using MonadThrow[F]) = self.toList[F, O]

    def take(n: Int): Stream[F, O] = self.take(n).void

    def filter(p: O => Boolean): Stream[F, O] = self.filter(p)

    def ++(that: => Stream[F, O]): Stream[F, O] =
      self >> that

    def repeat: Stream[F, O] =
      self.repeat

    def mapEval[O2](f: O => F[O2]): Stream[F, O2] =
      self.flatMapOutput(o => Stream.eval(f(o)))

    def run(using MonadThrow[F]): F[Unit] =
      fold(())((_, _) => ()).map(_(1))

    def handleErrorWith(handler: Throwable => Stream[F, O]): Stream[F, O] =
      Pull.Handle(self, handler)

    def onComplete(that: => Stream[F, O]): Stream[F, O] =
      handleErrorWith(t => that ++ raiseError(t)) ++ that

    def drain: Stream[F, Nothing] = 
      self.flatMapOutput(o => Stream.empty)

    def scope: Stream[F, O] = 
      Pull.OpenScope(self, None)
  end extension

  extension [O](self: Stream[Nothing, O])
    def fold[A](init: A)(f: (A, O) => A): A =
      self.fold(init)(f)(using MonadThrow[TailRec]).result(1)

    def toList: List[O] = self.toList(using MonadThrow[TailRec]).result

  given [F[_]]: Monad[[x] =>> Stream[F, x]] with
    def pure[A](x: A): Stream[F, A] = Pull.Output(x)
    def flatMap[A, B](fa: Stream[F, A])(f: A => Stream[F, B]): Stream[F, B] =
      fa.flatMapOutput(f)

    def tailRecM[A, B](a: A)(f: A => Stream[F, Either[A, B]]): Stream[F, B] =
      f(a).flatMapOutput {
        case Left(a)  => tailRecM(a)(f)
        case Right(o) => pure(o)
      }

  given MonadThrow[TailRec] with
    def pure[A](x: A): TailRec[A] = TailCalls.done(x)
    def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => TailRec[Either[A, B]]): TailRec[B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => TailCalls.done(b)
      }

    def handleErrorWith[A](fa: TailRec[A])(
        f: Throwable => TailRec[A]
    ): TailRec[A] =
      Try(fa.result).fold(f, TailCalls.done)
    def raiseError[A](e: Throwable): TailRec[A] =
      TailCalls.done(throw e)
end Stream

type Pipe[F[_], -I, +O] = Stream[F, I] => Stream[F, O]
