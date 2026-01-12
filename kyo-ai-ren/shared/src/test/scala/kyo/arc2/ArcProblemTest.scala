package kyo.arc2

import grid.Grid
import grid.ops.Fill
import grid.ops.Geometry
import grid.ops.Objects
import grid.ops.Physics
import kyo.*
import kyo.ai.json.*
import kyo.arc2.grid.*
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import scala.io.Source

class ArcProblemTest extends Test:

    "ARC-AGI Problems" - {

        "05269061 - Diagonal pattern fill" in run {
            testProblem("05269061") {
                Fill.FillFromDiagonalPattern(backgroundColor = 0)
            }
        }

        "1cf80156 - Crop to object bounding box" in run {
            testProblem("1cf80156") {
                Geometry.CropToBounds(backgroundColor = 0)
            }
        }

        "67a3c6ac - Horizontal flip" in run {
            testProblem("67a3c6ac") {
                Geometry.Flip(Axis.Vertical)
            }
        }

        "1e0a9b12 - Gravity stacking" in run {
            testProblem("1e0a9b12") {
                Physics.ApplyGravity(Direction.Down, backgroundColor = 0)
            }
        }

        // "00576224 - Concatenate with horizontal flip" in run {
        //     testProblem("00576224") { input =>
        //         Combine.Concatenate(
        //             Geometry.Tile(input, timesX = 3, timesY = 1),
        //             Combine.Concatenate(
        //                 Geometry.Flip(Geometry.Tile(input, timesX = 3, timesY = 1), Axis.Vertical),
        //                 Geometry.Tile(input, timesX = 3, timesY = 1),
        //                 Axis.Horizontal
        //             ),
        //             Axis.Horizontal
        //         )
        //     }
        // }

        "3c9b0459 - 180 degree rotation" in run {
            testProblem("3c9b0459") {
                Geometry.Rotate(Rotation.R180)
            }
        }

    }

    private def loadProblem(problemId: String): ArcProblem =
        val resourceName = s"arc2/training/$problemId.json"
        val stream       = getClass.getClassLoader.getResourceAsStream(resourceName)
        require(stream != null, s"Resource not found: $resourceName")
        val jsonStr = Source.fromInputStream(stream).mkString
        Json.decode[ArcProblem](jsonStr).getOrThrow
    end loadProblem

    def testProblem(problemId: String)(op: Grid.Op): Assertion < Sync =
        val problem = loadProblem(problemId)

        problem.train.zipWithIndex.foreach { (task, idx) =>
            val input    = Grid(task.input)
            val expected = Grid(task.output)
            val result   = op(List(input))
            assert(result.size == 1)
            assert(
                result.head.cells == expected.cells,
                s"Training example $idx failed for problem $problemId"
            )
        }

        problem.test.zipWithIndex.foreach { (task, idx) =>
            val input = Grid(task.input)
            println(input.toMarkdown)
            val expected = Grid(task.output)
            val result   = op(List(input))
            assert(result.size == 1)
            assert(
                result.head.cells == expected.cells,
                s"Test case $idx failed for problem $problemId"
            )
        }

        succeed
    end testProblem
end ArcProblemTest
