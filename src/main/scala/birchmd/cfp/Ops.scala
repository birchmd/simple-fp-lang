package birchmd.cfp

import cats.Monad
import cats.effect.{Concurrent, Fiber}
import cats.implicits._

object Ops {
  def add(a: VInt, b: VInt): VInt = VInt(a.value + b.value)
  def eq(a: VInt, b: VInt): VBool = VBool(a.value == b.value)

  def map[F[_]: Monad](list: VList, f: VFunc[F])(
      implicit outer: Scope[F]
  ): F[VList] =
    for {
      result <- list.values.traverse(app[F](f, _))
    } yield VList(result)

  def getEl(list: VList, index: VInt): Value = list.values(index.value)

  def ifelse[F[_]](
      cond: VBool,
      trueBody: F[Value],
      falseBody: F[Value]
  ): F[Value] =
    if (cond.value) trueBody
    else falseBody

  def app[F[_]: Monad](f: VFunc[F], value: Value)(
      implicit outer: Scope[F]
  ): F[Value] =
    for {
      scope <- outer.subscope
      result <- f.f(value, scope)
    } yield result

  def start[F[_]: Concurrent: Monad](f: VFunc[F], value: Value)(
      implicit outer: Scope[F]
  ): F[VFiber[F]] =
    for {
      fiber <- Concurrent[F].start(app[F](f, value))
    } yield VFiber(fiber)

  def join[F[_]](vfib: VFiber[F]): F[Value] = vfib.fiber.join
}
