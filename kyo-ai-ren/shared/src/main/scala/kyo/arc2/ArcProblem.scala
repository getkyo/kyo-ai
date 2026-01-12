package kyo.arc2

import kyo.*
import kyo.ai.json.Json
import kyo.ren.evolution.Evolution.TestCase

case class ArcProblem(
    train: List[ArcProblem.Task],
    test: List[ArcProblem.Task]
) derives CanEqual, Json

object ArcProblem:

    enum Version:
        case V1, V2

    case class Task(
        input: List[List[Int]],
        output: List[List[Int]]
    ) derives CanEqual, Json

    def training(version: Version)(using Frame): List[TestCase] < Sync =
        loadDirectory(version, "training")

    def evaluation(version: Version)(using Frame): List[TestCase] < Sync =
        loadDirectory(version, "evaluation")

    private def loadDirectory(version: Version, folder: String)(using Frame): List[TestCase] < Sync =
        for
            files <-
                Path("/Users/fwbrasil/workspace/kyo-ai/kyo-ai-ren/shared/src/main/resources/arc", version.toString.toLowerCase, folder).list(
                    "json"
                )
            files     <- Random.withSeed(56688)(Random.shuffle(files)).map(_.drop(2).take(5))
            _         <- Console.printLine(files.map(_.parts.last).toList)
            testCases <- Kyo.foreach(files)(file => file.read.map(TestCase(file.parts.last.toString, _, "")))
        yield List.from(testCases)
        end for
    end loadDirectory
end ArcProblem
