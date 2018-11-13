package birchmd.cfp.example

import birchmd.cfp._

import cats.Monad
import cats.effect.Concurrent
import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler

/*
 * let fib = \n -> {
 *   let inner = \n -> \a -> \b -> {
 *     if (n == 0) return a
 *     else return inner (n - 1) b (a + b)
 *   }
 *   return inner n 0 1
 * }
 * return fib 7
 */
object FibExample {
  def main(args: Array[String]): Unit = {
    val example =
      for {
        topScope <- Scope.empty[Task]
        _ <- topScope.bind(
          "fib",
          VFunc[Task](
            (n: Value, s: Scope[Task]) =>
              for {
                _ <- s.bind(
                  "inner",
                  VFunc[Task](
                    (n: Value, s: Scope[Task]) =>
                      Task.pure(
                        VFunc[Task](
                          (a: Value, s: Scope[Task]) =>
                            Task.pure(
                              VFunc[Task](
                                (b: Value, s: Scope[Task]) =>
                                  Ops.ifelse(
                                    Ops.eq(n.asInstanceOf[VInt], VInt(0)),
                                    Task.pure(a),
                                    for {
                                      inner <- s.deref("inner")
                                      app1 <- Ops.app(
                                        inner.asInstanceOf[VFunc[Task]],
                                        Ops.add(n.asInstanceOf[VInt], VInt(-1))
                                      )(Monad[Task], s)
                                      app2 <- Ops
                                        .app(app1.asInstanceOf[VFunc[Task]], b)(
                                          Monad[Task],
                                          s
                                        )
                                      app3 <- Ops.app(
                                        app2.asInstanceOf[VFunc[Task]],
                                        Ops.add(
                                          a.asInstanceOf[VInt],
                                          b.asInstanceOf[VInt]
                                        )
                                      )(Monad[Task], s)
                                    } yield app3
                                  )
                              )
                            )
                        )
                      )
                  )
                )
                inner <- s.deref("inner")
                app1 <- Ops.app(
                  inner.asInstanceOf[VFunc[Task]],
                  n.asInstanceOf[VInt]
                )(Monad[Task], s)
                app2 <- Ops
                  .app(app1.asInstanceOf[VFunc[Task]], VInt(0))(
                    Monad[Task],
                    s
                  )
                app3 <- Ops.app(
                  app2.asInstanceOf[VFunc[Task]],
                  VInt(1)
                )(Monad[Task], s)
              } yield app3
          )
        )
        fib <- topScope.deref("fib")
        r <- Ops.app[Task](fib.asInstanceOf[VFunc[Task]], VInt(7))(
          Monad[Task],
          topScope
        )
      } yield r

    implicit val scheduler = Scheduler.io("example")
    println(example.runSyncUnsafe())
  }
}
