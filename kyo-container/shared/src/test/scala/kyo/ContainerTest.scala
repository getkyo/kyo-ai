package kyo

import kyo.debug.Debug

class ContainerTest extends Test:

    val config = Container.Config("alpine", "tail -f /dev/null", stopTimeout = 100.millis)

    "start" in run {
        for
            c <- Container.init(config)
            s <- c.state
        yield
            assert(c.id.nonEmpty)
            assert(s == Container.State.Running)
    }

    "start stop" in run {
        for
            c  <- Container.init(config)
            s1 <- c.state
            _  <- c.stop
            s2 <- Abort.run(c.state)
        yield
            assert(c.id.nonEmpty)
            assert(s1 == Container.State.Running)
            assert(s2.isFailure)
    }

    "execute command" in run {
        for
            c <- Container.init(config)
            r <- c.execute("echo test")
        yield assert(r.trim == "test")
    }

    "copyTo and copyFrom transfer files" in run {
        val testContent   = "test content"
        val sourcePath    = Path("/tmp/test-source-" + java.lang.System.currentTimeMillis)
        val containerPath = Path("/tmp/container-file")
        val destPath      = Path("./test-dest-" + java.lang.System.currentTimeMillis)

        for
            c           <- Container.init(config)
            _           <- sourcePath.write(testContent)
            _           <- c.copyTo(sourcePath, containerPath)
            catResult   <- c.execute(s"cat ${containerPath.toJava}")
            _           <- c.copyFrom(containerPath, destPath)
            destContent <- destPath.read
            _           <- sourcePath.remove
            _           <- destPath.remove
        yield
            assert(catResult.trim == testContent)
            assert(destContent == testContent)
        end for
    }

    "wait for health check" in run {
        val healthCheckConfig = config.copy(
            waitFor = Container.Config.WaitFor.healthCheck(
                Schedule.fixed(100.millis).take(10)
            )
        )

        for
            c <- Container.init(healthCheckConfig)
            s <- c.state
        yield assert(s == Container.State.Running)
        end for
    }

end ContainerTest
