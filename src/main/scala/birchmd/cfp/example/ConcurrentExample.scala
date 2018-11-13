package birchmd.cfp.example

import birchmd.cfp._

import cats.Monad
import cats.effect.Concurrent
import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler

/*
 * let x = 1
 * let f = \y -> { return y + 3 }
 * let g = \y -> { return y + 1 }
 * let a = [start f x, start g x]
 * let b = map a (\f -> join f)
 * return (get b 0) + (get b 1)
 */
object ConcurrentExample {
  def main(args: Array[String]): Unit = {
    val example =
      for {
        topScope <- Scope.empty[Task]
        _ <- topScope.bind("x", VInt(1))
        _ <- topScope.bind(
          "f",
          VFunc[Task](
            (y: Value, s: Scope[Task]) =>
              Task.pure(Ops.add(y.asInstanceOf[VInt], VInt(3)))
          )
        )
        _ <- topScope.bind(
          "g",
          VFunc[Task](
            (y: Value, s: Scope[Task]) =>
              Task.pure(Ops.add(y.asInstanceOf[VInt], VInt(1)))
          )
        )
        f <- topScope.deref("f")
        x <- topScope.deref("x")
        g <- topScope.deref("g")
        _ <- topScope.bindF(
          "a",
          List(
            Ops.start[Task](f.asInstanceOf[VFunc[Task]], x)(
              Concurrent[Task],
              Monad[Task],
              topScope
            ),
            Ops.start[Task](g.asInstanceOf[VFunc[Task]], x)(
              Concurrent[Task],
              Monad[Task],
              topScope
            )
          ).sequence
            .map(VList.apply)
        )
        a <- topScope.deref("a")
        _ <- topScope.bindF(
          "b",
          Ops
            .map[Task](
              a.asInstanceOf[VList],
              VFunc[Task](
                (f: Value, s: Scope[Task]) =>
                  Ops.join[Task](f.asInstanceOf[VFiber[Task]])
              )
            )(Monad[Task], topScope)
            .map(_.asInstanceOf[Value])
        )
        b <- topScope.deref("b")
      } yield
        Ops.add(
          Ops.getEl(b.asInstanceOf[VList], VInt(0)).asInstanceOf[VInt],
          Ops.getEl(b.asInstanceOf[VList], VInt(1)).asInstanceOf[VInt]
        )

    implicit val scheduler = Scheduler.io("example")
    println(example.runSyncUnsafe())
  }
}
