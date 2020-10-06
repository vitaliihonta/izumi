package izumi.functional.bio.syntax

import izumi.functional.bio._
import izumi.functional.bio.syntax.Syntax2.ImplicitPuns
import izumi.fundamentals.platform.language.unused

trait Syntax1 extends ImplicitPuns

object Syntax1 {

  class PanicOps[F[+_], +A, E](protected[this] val r: F[A])(implicit protected[this] val F: Panic1[F, E]) {
    @inline final def map[B](f: A => B): F[B] = F.map(r)(f)

    @inline final def as[B](b: => B): F[B] = F.map(r)(_ => b)
    @inline final def void: F[Unit] = F.void(r)
    @inline final def widen[A1](implicit @unused ev: A <:< A1): F[A1] = r.asInstanceOf[F[A1]]

    /** execute two operations in order, return result of second operation */
    @inline final def *>[B](f0: => F[B]): F[B] = F.*>(r, f0)

    /** execute two operations in order, same as `*>`, but return result of first operation */
    @inline final def <*[B](f0: => F[B]): F[A] = F.<*(r, f0)

    /** execute two operations in order, return result of both operations */
    @inline final def zip[B, C](r2: => F[B]): F[(A, B)] = F.map2(r, r2)(_ -> _)

    /** execute two operations in order, map their results */
    @inline final def map2[B, C](r2: => F[B])(f: (A, B) => C): F[C] = F.map2(r, r2)(f)

    @inline final def forever: F[Nothing] = F.forever(r)

    @inline final def guarantee(cleanup: F[Unit]): F[A] = F.guarantee(r, cleanup)

    @inline final def orElse[A1 >: A](r2: => F[A1]): F[A1] = F.orElse(r, r2)

    @inline final def flatMap[B](f0: A => F[B]): F[B] = F.flatMap(r)(f0)
    @inline final def tap[B](f0: A => F[Unit]): F[A] = F.tap(r, f0)

    @inline final def flatten[A1](implicit ev: A <:< F[A1]): F[A1] = F.flatten(new PanicOps(r).widen)

    // FIXME: bullshit
    @inline final def leftMap(f: E => E): F[A] = F.leftMap(r)(f)
//    @inline final def leftMap(f: Nothing => Nothing): F[A] = F.leftMap(r)(f)
//    @inline final def leftMap(f: Nothing => Nothing): F[A] = F.leftMap[Any, Nothing, A, Nothing](r)(f)

//    // FIXME: bullshit
//    @inline final def bimap[B](f: E => E, g: A => B): F[B] = F.bimap(r)(f, g)
//    // FIXME: bullshit
//    @inline final def leftMap2[A1 >: A](r2: => F[A1])(f: (E, E) => E): F[A1] = F.leftMap2(r, r2)(f)
//    // FIXME: bullshit
//    @inline final def catchAll[A2 >: A](h: E => F[A2]): F[A2] = F.catchAll[Any, E, A2, E](r)(h)
//    // FIXME: bullshit
//    @inline final def catchSome[A2 >: A](h: PartialFunction[E, F[A2]]): F[A2] = F.catchSome[Any, E, A2, E](r)(h)
//    // FIXME: bullshit
//    @inline final def redeem[B](err: E => F[B], succ: A => F[B]): F[B] = F.redeem[Any, E, A, E, B](r)(err, succ)
//    // FIXME: bullshit
//    @inline final def redeemPure[B](err: E => B, succ: A => B): F[B] = F.redeemPure(r)(err, succ)
//    // FIXME: bullshit
//    @inline final def attempt: F[Either[E, A]] = F.attempt(r)
//    // FIXME: bullshit
//    @inline final def tapError(f: E => F[Unit]): F[A] = F.tapError[Any, E, A, E](r)(f)
//    // FIXME: bullshit
//    @inline final def leftFlatMap(f: E => F[E]): F[A] = F.leftFlatMap(r)(f)
//    // FIXME: bullshit
//    @inline final def flip: F[E] = F.flip(r)
//    // FIXME: bullshit
//    @inline final def tapBoth(err: E => F[Unit])(succ: A => F[Unit]): F[A] = F.tapBoth[Any, E, A, E](r)(err, succ)
//    // FIXME: bullshit
//    @inline final def fromEither[A1](implicit ev: A <:< Either[E, A1]): F[A1] = F.flatMap[Any, E, A, A1](r)(F.fromEither[E, A1](_))
//    // FIXME: bullshit
//    @inline final def fromOption[A1](errorOnNone: => E)(implicit ev1: A <:< Option[A1]): F[A1] = F.flatMap[Any, E, A, A1](r)(F.fromOption(errorOnNone)(_))
//    // FIXME: bullshit
//    @inline final def withFilter[E1 >: E](predicate: A => Boolean)(implicit filter: WithFilter[E1], pos: SourceFilePositionMaterializer): F[A] =
//      F.withFilter[Any, E1, A](r)(predicate)
//    // FIXME: bullshit
//    @inline final def bracket[B](release: A => F[Unit])(use: A => F[B]): F[B] = F.bracket(r: F[A])(release)(use)
//    // FIXME: bullshit
//    @inline final def bracketCase[B](release: (A, Exit[E, B]) => F[Unit])(use: A => F[B]): F[B] = F.bracketCase(r: F[A])(release)(use)
//    // FIXME: bullshit
////    @inline final def sandboxExit: F[Exit[E, A]] = F.redeemPure(F.sandbox(r))(identity[Exit[E, A]], Exit.Success(_))
//    @inline final def sandboxExit: F[Exit[Nothing, A]] = F.redeemPure(F.sandbox(r))(identity, Exit.Success(_))
//    // FIXME: bullshit
//    @inline final def guaranteeCase(cleanup: Exit[E, A] => F[Unit]): F[A] = F.guaranteeCase(r, cleanup)
//    // FIXME: bullshit
//    @inline final def orTerminate(implicit ev: E <:< Throwable): F[A] = F.catchAll[Any, E, A, E](r)(F.terminate(_))

//    @inline final def sandbox: F[Exit.Failure[E], A] = F.sandbox(r)
//
//    @deprecated("renamed to sandboxExit", "0.11")
//    @inline final def sandboxBIOExit = sandboxExit

    /**
      * Catch all _defects_ in this effect and convert them to Throwable
      * Example:
      *
      * {{{
      *   F.pure(1)
      *     .map(_ => ???)
      *     .sandboxThrowable
      *     .catchAll(_ => IO2(println("Caught error!")))
      * }}}
      */
//    @inline final def sandboxToThrowable(implicit ev: E <:< Throwable): F[A] =
//      F.leftMap(F.sandbox(r))(_.toThrowable)

  }

//  class IOOps[F[+_, +_], +E, +A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: IO2[F]) extends PanicOps(r) {
//    @inline final def bracketAuto[E1 >: E, B](use: A => F[E1, B])(implicit ev: A <:< AutoCloseable): F[E1, B] =
//      F.bracket[Any, E1, A, B](r)(c => F.sync(c.close()))(use)
//  }
//
//  class ParallelOps[F[+_, +_], +E, +A](protected[this] val r: F[E, A])(implicit protected[this] val F: Parallel2[F]) {
//    @inline final def zipWithPar[E1 >: E, B, C](that: F[E1, B])(f: (A, B) => C): F[E1, C] = F.zipWithPar(r, that)(f)
//    @inline final def zipPar[E1 >: E, B](that: F[E1, B]): F[E1, (A, B)] = F.zipPar(r, that)
//    @inline final def zipParLeft[E1 >: E, B](that: F[E1, B]): F[E1, A] = F.zipParLeft(r, that)
//    @inline final def zipParRight[E1 >: E, B](that: F[E1, B]): F[E1, B] = F.zipParRight(r, that)
//  }
//  final class ConcurrentOps[F[+_, +_], +E, +A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: Concurrent2[F])
//    extends ParallelOps(r)(F) {
//    @inline final def race[E1 >: E, A1 >: A](that: F[E1, A1]): F[E1, A1] = F.race(r, that)
//    @inline final def racePair[E1 >: E, A1 >: A](
//      that: F[E1, A1]
//    ): F[E1, Either[(A, Fiber2[F, E1, A1]), (Fiber2[F, E1, A], A1)]] = F.racePair(r, that)
//    @inline final def uninterruptible: F[E, A] = F.uninterruptible(r)
//  }
//  class AsyncOps[F[+_, +_], +E, +A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: Async2[F]) extends IOOps(r) {
//    @inline final def zipWithPar[E1 >: E, B, C](that: F[E1, B])(f: (A, B) => C): F[E1, C] = F.zipWithPar(r, that)(f)
//    @inline final def zipPar[E1 >: E, B](that: F[E1, B]): F[E1, (A, B)] = F.zipPar(r, that)
//    @inline final def zipParLeft[E1 >: E, B](that: F[E1, B]): F[E1, A] = F.zipParLeft(r, that)
//    @inline final def zipParRight[E1 >: E, B](that: F[E1, B]): F[E1, B] = F.zipParRight(r, that)
//
//    @inline final def race[E1 >: E, A1 >: A](that: F[E1, A1]): F[E1, A1] = F.race(r, that)
//    @inline final def racePair[E1 >: E, A1 >: A](
//      that: F[E1, A1]
//    ): F[E1, Either[(A, Fiber2[F, E1, A1]), (Fiber2[F, E1, A], A1)]] = F.racePair(r, that)
//    @inline final def uninterruptible: F[E, A] = F.uninterruptible(r)
//  }
//
//  final class TemporalOps[F[+_, +_], +E, +A](protected[this] val r: F[E, A])(implicit protected[this] val F: Temporal2[F]) {
//    @inline final def retryOrElse[A2 >: A, E2](duration: FiniteDuration, orElse: => F[E2, A2]): F[E2, A2] = F.retryOrElse[Any, E, A2, E2](r)(duration, orElse)
//    @inline final def repeatUntil[E2 >: E, A2](tooManyAttemptsError: => E2, sleep: FiniteDuration, maxAttempts: Int)(implicit ev: A <:< Option[A2]): F[E2, A2] =
//      F.repeatUntil[Any, E2, A2](new FunctorOps(r)(F.InnerF).widen)(tooManyAttemptsError, sleep, maxAttempts)
//
//    @inline final def timeout(duration: Duration): F[E, Option[A]] = F.timeout(duration)(r)
//    @inline final def timeoutFail[E1 >: E](e: E1)(duration: Duration): F[E1, A] = F.timeoutFail(duration)(e, r)
//  }
//
//  final class ForkOps[F[+_, +_], +E, +A](private val r: F[E, A])(implicit private val F: Fork2[F]) {
//    @inline final def fork: F[Nothing, Fiber2[F, E, A]] = F.fork(r)
//  }
//
//  trait ImplicitPuns extends ImplicitPuns1 {
//    @inline implicit final def Temporal2[F[+_, +_]: Temporal2, E, A](self: F[E, A]): TemporalOps[F, E, A] = new TemporalOps[F, E, A](self)
//    @inline implicit final def Temporal2[F[+_, +_]: Error2, E, A](self: F[E, A]): ErrorOps[F, E, A] = new ErrorOps[F, E, A](self)
//    @inline final def Temporal2[F[+_, +_]: Temporal2]: Temporal2[F] = implicitly
//
//    @inline implicit final def Fork2[F[+_, +_]: Fork2, E, A](self: F[E, A]): ForkOps[F, E, A] = new ForkOps[F, E, A](self)
//    @inline final def Fork2[F[+_, +_]: Fork2]: Fork2[F] = implicitly
//  }
//  trait ImplicitPuns1 extends ImplicitPuns2 {
//    @inline implicit final def Async2[F[+_, +_]: Async2, E, A](self: F[E, A]): AsyncOps[F, E, A] = new AsyncOps[F, E, A](self)
//    @inline final def Async2[F[+_, +_]: Async2]: Async2[F] = implicitly
//  }
//  trait ImplicitPuns2 extends ImplicitPuns3 {
//    @inline implicit final def Concurrent2[F[+_, +_]: Concurrent2, E, A](self: F[E, A]): ConcurrentOps[F, E, A] = new ConcurrentOps[F, E, A](self)
//    @inline implicit final def Concurrent2[F[+_, +_]: Panic2, E, A](self: F[E, A]): PanicOps[F, E, A] = new PanicOps[F, E, A](self)
//    @inline final def Concurrent2[F[+_, +_]: Concurrent2]: Concurrent2[F] = implicitly
//  }
//  trait ImplicitPuns3 extends ImplicitPuns4 {
//    @inline implicit final def Parallel2[F[+_, +_]: Parallel2, E, A](self: F[E, A]): ParallelOps[F, E, A] = new ParallelOps[F, E, A](self)
//    @inline implicit final def Parallel2[F[+_, +_]: Monad2, E, A](self: F[E, A]): MonadOps[F, E, A] = new MonadOps[F, E, A](self)
//    @inline final def Parallel2[F[+_, +_]: Parallel2]: Parallel2[F] = implicitly
//  }
//  trait ImplicitPuns4 extends ImplicitPuns5 {
//    @inline implicit final def IO2[F[+_, +_]: IO2, E, A](self: F[E, A]): IOOps[F, E, A] = new IOOps[F, E, A](self)
//    /**
//      * Shorthand for [[IO3#syncThrowable]]
//      *
//      * {{{
//      *   IO2(println("Hello world!"))
//      * }}}
//      */
//    @inline final def IO2[F[+_, +_], A](effect: => A)(implicit F: IO2[F]): F[Throwable, A] = F.syncThrowable(effect)
//    @inline final def IO2[F[+_, +_]: IO2]: IO2[F] = implicitly
//  }
//  trait ImplicitPuns5 extends ImplicitPuns6 {
//    @inline implicit final def Panic2[F[+_, +_]: Panic2, E, A](self: F[E, A]): PanicOps[F, E, A] = new PanicOps[F, E, A](self)
//    @inline final def Panic2[F[+_, +_]: Panic2]: Panic2[F] = implicitly
//  }
//  trait ImplicitPuns6 extends ImplicitPuns7 {
//    @inline implicit final def Bracket2[F[+_, +_]: Bracket2, E, A](self: F[E, A]): BracketOps[F, E, A] = new BracketOps[F, E, A](self)
//    @inline final def Bracket2[F[+_, +_]: Bracket2]: Bracket2[F] = implicitly
//  }
//  trait ImplicitPuns7 extends ImplicitPuns8 {
//    @inline implicit final def Error2[F[+_, +_]: Error2, E, A](self: F[E, A]): ErrorOps[F, E, A] = new ErrorOps[F, E, A](self)
//    @inline final def Error2[F[+_, +_]: Error2]: Error2[F] = implicitly
//  }
//  trait ImplicitPuns8 extends ImplicitPuns9 {
//    @inline implicit final def ApplicativeError2[F[+_, +_]: ApplicativeError2, E, A](self: F[E, A]): ApplicativeErrorOps[F, E, A] = new ApplicativeErrorOps[F, E, A](self)
//    @inline final def ApplicativeError2[F[+_, +_]: ApplicativeError2]: ApplicativeError2[F] = implicitly
//  }
//  trait ImplicitPuns9 extends ImplicitPuns10 {
//    @inline implicit final def Guarantee2[F[+_, +_]: Guarantee2, E, A](self: F[E, A]): GuaranteeOps[F, E, A] = new GuaranteeOps[F, E, A](self)
//    @inline final def Guarantee2[F[+_, +_]: Guarantee2]: Guarantee2[F] = implicitly
//  }
//  trait ImplicitPuns10 extends ImplicitPuns11 {
//    @inline implicit final def Monad2[F[+_, +_]: Monad2, E, A](self: F[E, A]): MonadOps[F, E, A] = new MonadOps[F, E, A](self)
//    @inline final def Monad2[F[+_, +_]: Monad2]: Monad2[F] = implicitly
//  }
//  trait ImplicitPuns11 extends ImplicitPuns12 {
//    @inline implicit final def Applicative2[F[+_, +_]: Applicative2, E, A](self: F[E, A]): ApplicativeOps[F, E, A] = new ApplicativeOps[F, E, A](self)
//    @inline final def Applicative2[F[+_, +_]: Applicative2]: Applicative2[F] = implicitly
//  }
//  trait ImplicitPuns12 extends ImplicitPuns13 {
//    @inline implicit final def Bifunctor2[F[+_, +_]: Bifunctor2, E, A](self: F[E, A]): BifunctorOps[F, E, A] = new BifunctorOps[F, E, A](self)
//    @inline implicit final def Bifunctor2[F[+_, +_]: Functor2, E, A](self: F[E, A]): FunctorOps[F, E, A] = new FunctorOps[F, E, A](self)
//    @inline final def Bifunctor2[F[+_, +_]: Bifunctor2]: Bifunctor2[F] = implicitly
//  }
//  trait ImplicitPuns13 extends BIOSyntax {
//    @inline implicit final def Functor2[F[+_, +_]: Functor2, E, A](self: F[E, A]): FunctorOps[F, E, A] = new FunctorOps[F, E, A](self)
//    @inline final def Functor2[F[+_, +_]: Functor2]: Functor2[F] = implicitly
//  }

}
