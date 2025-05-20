package kyo

import java.util.concurrent.CompletionStage
import java.util.concurrent.Flow.Publisher
import neotypes.AsyncDriver
import neotypes.AsyncTransaction
import neotypes.Driver
import neotypes.GraphDatabase
import neotypes.StreamDriver
import neotypes.StreamTransaction
import neotypes.TransactionConfig
import neotypes.mappers.ResultMapper
import neotypes.model.query.QueryParam
import neotypes.query.DeferredQueryBuilder
import neotypes.syntax.all.*
import org.neo4j.configuration.GraphDatabaseSettings
import org.neo4j.dbms.api.DatabaseManagementServiceBuilder
import scala.collection.Factory

opaque type Graph <: Env[Graph.Runtime] & Async & Resource = Env[Graph.Runtime] & Async & Resource

export neotypes.syntax.all.c

object Graph:

    def run[A, S](driver: AsyncDriver[F] < S, config: TransactionConfig = TransactionConfig.default)(
        v: A < (Graph & S)
    )(using Frame): A < (Async & Resource & S) =
        driver.map(d => Env.run(Runtime(d, config, Absent))(v))

    def query[Out: ResultMapper](string: String)(using Frame): Chunk[Out] < Graph =
        query(Chunk, string, Map.empty)

    def query[Out: ResultMapper](string: String, params: Map[String, QueryParam])(using Frame): Chunk[Out] < Graph =
        withTransaction(_.collectAs(string, params, ResultMapper[Out], Chunk).map(_._1))

    def query[Out: ResultMapper](using Frame)[T, C](factory: Factory[Out, C], string: String): C < Graph =
        query(factory, string, Map.empty)

    def query[Out: ResultMapper](using Frame)[T, C](factory: Factory[Out, C], string: String, params: Map[String, QueryParam]): C < Graph =
        withTransaction(_.collectAs(string, params, ResultMapper[Out], factory).map(_._1))

    def query[Out: ResultMapper](builder: DeferredQueryBuilder)(using Frame): Chunk[Out] < Graph =
        query[Out](Chunk, builder)

    def query[Out: ResultMapper](using Frame)[T, C](factory: Factory[Out, C], builder: DeferredQueryBuilder): C < Graph =
        withTransaction(tx => builder.query(ResultMapper[Out]).collectAs[F, C](factory, tx))

    def execute(string: String, params: Map[String, QueryParam] = Map.empty)(using Frame): Unit < Graph =
        withTransaction(_.execute(string, params).unit)

    def execute(builder: DeferredQueryBuilder)(using Frame): Unit < Graph =
        withTransaction(builder.execute.void)

    def withConfig[A, S](config: TransactionConfig)(v: A < S)(using Frame): A < (Graph & S) =
        Env.use[Runtime] { r =>
            Env.run(r.copy(config = config))(v)
        }

    private def withTransaction[A, S](f: AsyncTransaction[F] => A < S)(using Frame): A < (Graph & S) =
        Env.use[Runtime] {
            case Runtime(driver, config, Present(tx)) =>
                f(tx)
            case Runtime(driver, config, _) =>
                transaction(withTransaction(f))
        }

    def transaction[A, S](v: => A < S)(using Frame): A < (Graph & S) =
        Env.use[Runtime] {
            case Runtime(driver, config, Absent) =>
                for
                    done <- AtomicBoolean.init
                    tx   <- driver.transaction(config)
                    _ <- Resource.ensure(
                        done.use {
                            case false => Resource.run(tx.rollback)
                            case true  => ()
                        }
                    )
                    result <-
                        v.handle(
                            Env.run(Runtime(driver, config, Present(tx))),
                            Abort.run
                        )
                    _ <- done.set(true)
                yield result match
                    case Result.Success(value) =>
                        tx.commit.andThen(value)
                    case Result.Error(e) =>
                        tx.rollback.andThen(Abort.panic(e))
            case _ =>
                v
        }

    opaque type F[A] <: (A < (Async & Resource)) = A < (Async & Resource)

    object F:
        implicit final val instance: neotypes.Async.Aux[F, F] =
            given Frame = Frame.internal
            new neotypes.Async[F]:
                final override type R[A] = A < (Async & Resource)

                final override def fromCompletionStage[A](completionStage: => CompletionStage[A]) =
                    Fiber.fromCompletionStage(completionStage)

                final override def delay[A](a: => A) =
                    IO(a)

                final override def flatMap[A, B](task: F[A])(f: A => F[B]) =
                    task.map(f)

                final override def fromEither[A](e: => Either[Throwable, A]) =
                    IO {
                        e match
                            case Left(ex)     => Abort.panic(ex)
                            case Right(value) => value
                    }

                override def guarantee[A, B](task: F[A])(f: A => F[B])(
                    finalizer: (A, Option[Throwable]) => F[Unit]
                ) =
                    for
                        done <- AtomicBoolean.init
                        a    <- task
                        _ <- Resource.ensure {
                            done.use {
                                case true  => ()
                                case false => Resource.run(finalizer(a, None))
                            }
                        }
                        result <- Abort.run(f(a))
                        _      <- done.set(true)
                    yield result match
                        case Result.Success(v) =>
                            finalizer(a, None).andThen(v)
                        case Result.Error(e) =>
                            finalizer(a, Option(e)).andThen(Abort.panic(e))

                final override def map[A, B](task: F[A])(f: A => B) =
                    task.map(f(_))

                final override def mapError[A](task: F[A])(f: Throwable => Throwable): F[A] =
                    task.handle(
                        Abort.recoverError[Throwable](ex => Abort.panic(f(ex.failureOrPanic)))
                    )

                override def resource[A](input: => A)(close: A => F[Unit]) =
                    Resource.acquireRelease(input)(a => Resource.run(close(a)))
            end new
        end instance
    end F

    case class Runtime(
        driver: AsyncDriver[F],
        config: TransactionConfig,
        transaction: Maybe[AsyncTransaction[F]]
    )
end Graph
