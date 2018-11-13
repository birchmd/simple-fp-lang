package birchmd.cfp

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait Scope[F[_]] {
  import Scope.Label

  def bind(name: Label, value: Value): F[Unit]
  def bindF(name: Label, value: F[Value]): F[Unit]
  def deref(name: Label): F[Value]

  //creates a new scope were deref will pick out values from this scope, but new bindings are not persisted here and shadowing is allowed
  def subscope: F[Scope[F]]
}

object Scope {
  type Label = String

  def apply[F[_]](implicit ev: Scope[F]): Scope[F] = ev

  private def fromRef[F[_]: Sync](
      ref: Ref[F, Map[Label, Value]],
      parent: Option[Scope[F]]
  ): Scope[F] = new Scope[F] {
    def bind(name: Label, value: Value): F[Unit] =
      ref.update(_ + (name -> value))
    def bindF(name: Label, value: F[Value]): F[Unit] =
      for {
        v <- value
        _ <- bind(name, v)
      } yield ()
    def deref(name: Label): F[Value] = ref.get.map(_.get(name)).flatMap {
      case Some(value) => value.pure[F]
      case None =>
        parent.traverse(_.deref(name)).flatMap {
          case Some(value) => value.pure[F]
          case None =>
            Sync[F].raiseError(new Exception(s"$name not bound to any value!"))
        }
    }

    def subscope: F[Scope[F]] =
      for {
        childRef <- Ref.of[F, Map[Label, Value]](Map.empty[Label, Value])
      } yield fromRef[F](childRef, Some(this))
  }

  def empty[F[_]: Sync]: F[Scope[F]] =
    Ref.of[F, Map[Label, Value]](Map.empty[Label, Value]).map(fromRef(_, None))
}
