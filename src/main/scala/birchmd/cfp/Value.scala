package birchmd.cfp

import cats.effect.Fiber

sealed trait Value

case object Void extends Value
case class VInt(value: Int) extends Value
case class VBool(value: Boolean) extends Value
case class VList(values: List[Value]) extends Value
case class VFunc[F[_]](f: (Value, Scope[F]) => F[Value]) extends Value
case class VFiber[F[_]](fiber: Fiber[F, Value]) extends Value
