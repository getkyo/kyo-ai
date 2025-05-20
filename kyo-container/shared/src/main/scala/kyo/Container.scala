package kyo

import java.util.concurrent.TimeUnit
import kyo.Container.Config
import kyo.debug.Debug

case class Container(id: String, config: Config):

    def state(using Frame): Container.State < (Async & Abort[ContainerException]) =
        Container.Service.use(_.state(id))

    def execute(command: String)(using Frame): String < (Async & Abort[ContainerException]) =
        Container.Service.use(_.execute(id, command))

    def copyTo(source: Path, destination: Path)(using Frame): Unit < (Async & Abort[ContainerException]) =
        Container.Service.use(_.copyTo(id, source, destination))

    def copyFrom(source: Path, destination: Path)(using Frame): Unit < (Async & Abort[ContainerException]) =
        Container.Service.use(_.copyFrom(id, source, destination))

    def stop(using Frame): Unit < (Async & Abort[ContainerException]) =
        Container.Service.use(_.stop(id, config.stopTimeout)).andThen(
            if config.destroyOnStop then destroy
            else ()
        )

    def destroy(using Frame): Unit < (Async & Abort[ContainerException]) =
        Container.Service.use(_.destroy(id))

    def logs(using Frame): String < (Async & Abort[ContainerException]) =
        Container.Service.use(_.logs(id))

end Container

case class ContainerException(cause: String | Throwable)(using Frame) extends KyoException("Container unavailable", cause)

object Container:
    enum State derives CanEqual:
        case Running, Stopped, Failed

    case class Config(
        image: String,
        startCommand: String = "",
        name: Maybe[String] = Absent,
        ports: List[Config.Port] = Nil,
        env: Map[String, String] = Map.empty,
        volumes: List[Config.Volume] = Nil,
        waitFor: Config.WaitFor = Config.WaitFor.default,
        destroyOnStop: Boolean = true,
        stopTimeout: Duration = 5.seconds
    ) derives CanEqual

    object Config:
        case class Port(host: Int, container: Int)
        case class Volume(host: Path, container: Path)

        sealed trait WaitFor:
            def apply(container: Container)(using Frame): Unit < (Async & Abort[ContainerException])

        object WaitFor:
            val defaultRetrySchedule = Schedule.fixed(1.second).take(60)

            val default = healthCheck()

            def healthCheck(
                retrySchedule: Schedule = defaultRetrySchedule,
                command: String = "echo healthy",
                expectedOutput: String = "healthy"
            ): WaitFor =
                new WaitFor:
                    def apply(container: Container)(using frame: Frame) =
                        Retry[ContainerException](retrySchedule) {
                            container.execute(command).map { output =>
                                if output == expectedOutput then ()
                                else Abort.fail(ContainerException("Container not healthy yet"))
                            }
                        }

            case class LogMessage(
                message: String,
                retrySchedule: Schedule = defaultRetrySchedule
            ) extends WaitFor:
                def apply(container: Container)(using frame: Frame) =
                    Retry[ContainerException](retrySchedule) {
                        container.logs.map { logs =>
                            if logs.contains(message) then ()
                            else Abort.fail(ContainerException(s"Container not healthy yet. Log message '$message' not found"))
                        }
                    }
            end LogMessage

            case class Port(
                port: Int,
                retrySchedule: Schedule = defaultRetrySchedule
            ) extends WaitFor:
                def apply(container: Container)(using Frame) =
                    Retry[ContainerException](retrySchedule) {
                        container.execute(s"nc -z localhost $port").unit
                    }
            end Port
        end WaitFor
    end Config

    def init(config: Config)(using Frame): Container < (Async & Abort[ContainerException] & Resource) =
        Service.use { service =>
            for
                id <- service.create(config, config.startCommand)
                _  <- service.start(id)
                container = new Container(id, config)
                _ <- Resource.ensure {
                    Log.debug(s"Stopping container $id").andThen(
                        container.stop
                    )
                }
                _ <- config.waitFor(container)
            yield container
        }

    def init(
        image: String,
        startCommand: String = "tail -f /dev/null",
        name: Maybe[String] = Absent,
        ports: List[(Int, Int)] = Nil,
        env: Map[String, String] = Map.empty,
        volumes: List[(Path, Path)] = Nil,
        waitFor: Config.WaitFor = Config.WaitFor.healthCheck(),
        destroyOnStop: Boolean = true,
        stopTimeout: Duration = 5.seconds
    )(using Frame): Container < (Async & Abort[ContainerException] & Resource) =
        init(Config(
            image,
            startCommand,
            name,
            ports.map((h, c) => Config.Port(h, c)),
            env,
            volumes.map((h, c) => Config.Volume(h, c)),
            waitFor,
            destroyOnStop,
            stopTimeout
        ))

    abstract class Service:
        def create(config: Config, startCommand: String)(using Frame): String < (Async & Abort[ContainerException])
        def start(id: String)(using Frame): Unit < (Async & Abort[ContainerException])
        def stop(id: String, timeout: Duration)(using Frame): Unit < (Async & Abort[ContainerException])
        def destroy(id: String)(using Frame): Unit < (Async & Abort[ContainerException])
        def state(id: String)(using Frame): State < (Async & Abort[ContainerException])
        def execute(id: String, command: String)(using Frame): String < (Async & Abort[ContainerException])
        def copyTo(id: String, source: Path, destination: Path)(using Frame): Unit < (Async & Abort[ContainerException])
        def copyFrom(id: String, source: Path, destination: Path)(using Frame): Unit < (Async & Abort[ContainerException])
        def logs(id: String)(using Frame): String < (Async & Abort[ContainerException])
    end Service

    object Service:
        val docker: Service = ProcessService("docker")
        val podman: Service = ProcessService("podman")

        private val local = Local.init[Maybe[Service]](Maybe.empty)

        def let[A, S](service: Service)(v: => A < S)(using Frame): A < S =
            local.let(Maybe(service))(v)

        def use[A, S](f: Service => A < S)(using Frame): A < (Async & Abort[ContainerException] & S) =
            local.use {
                case Absent           => detectService.map(f)
                case Present(service) => f(service)
            }

        class ProcessService(commandName: String) extends Service:
            private def runCommand(args: String*)(using frame: Frame) =
                for
                    _       <- Log.debug((commandName +: args).mkString(" "))
                    process <- Process.Command((commandName +: args)*).spawn
                    exit    <- process.waitFor
                yield
                    if exit == 0 then
                        new String(process.stdout.readAllBytes()).trim
                    else
                        val stderr = new String(process.stderr.readAllBytes())
                        val stdout = new String(process.stdout.readAllBytes())
                        Abort.fail(ContainerException(
                            "Command '" + (commandName +: args).mkString(" ") + "' failed. " + stderr
                        ))
            end runCommand

            def create(config: Config, startCommand: String)(using Frame) =
                val createArgs = Seq("create") ++
                    config.name.map(n => Seq("--name", n)).getOrElse(Seq.empty) ++
                    config.ports.flatMap(p => Seq("-p", s"${p.host}:${p.container}")) ++
                    config.env.flatMap((k, v) => Seq("-e", s"$k=$v")) ++
                    config.volumes.flatMap(v => Seq("-v", s"${v.host}:${v.container}")) ++
                    Seq(config.image) ++
                    startCommand.split(" ").filter(_.nonEmpty).toSeq
                runCommand(createArgs*).map(_.trim)
            end create

            def start(id: String)(using Frame) = runCommand("start", id).unit

            def stop(id: String, timeout: Duration)(using Frame) =
                runCommand("stop", "--timeout", timeout.toSeconds.toString(), id).unit

            def destroy(id: String)(using Frame) = runCommand("rm", id).unit

            def state(id: String)(using Frame) =
                runCommand("inspect", "-f", "{{.State.Status}}", id).map {
                    case "running" => State.Running
                    case "exited"  => State.Stopped
                    case _         => State.Failed
                }

            def execute(id: String, command: String)(using Frame) =
                runCommand(("exec" :: id :: command.split(" ").filter(_.nonEmpty).toList)*)

            def copyTo(id: String, source: Path, destination: Path)(using Frame) =
                runCommand("cp", source.toJava.toString(), s"$id:${destination.toJava}").unit

            def copyFrom(id: String, source: Path, destination: Path)(using Frame) =
                runCommand("cp", s"$id:${source.toJava}", destination.toJava.toString()).unit

            def logs(id: String)(using Frame) = runCommand("logs", id)
        end ProcessService

        private def detectService(using frame: Frame): Service < (IO & Abort[ContainerException]) =
            for
                podmanAvailable <- Process.Command("podman", "version").waitFor(1, TimeUnit.SECONDS)
                dockerAvailable <- Process.Command("docker", "version").waitFor(1, TimeUnit.SECONDS)
            yield
                if podmanAvailable then Service.podman
                else if dockerAvailable then Service.docker
                else Abort.fail(ContainerException("No supported container service found"))
    end Service

end Container
