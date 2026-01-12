package kyo.arc2

import ArcEval.*
import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.grid.Grid
import kyo.arc2.grid.GridTool
import kyo.ren.*
import kyo.ren.evolution.*
import kyo.ren.evolution.Evolution.*
import kyo.ren.network.Gate
import kyo.ren.network.Input

class ArcEval extends Eval:
    private val maxRefinements = 1

    def apply(mind: Mind, test: TestCase)(using Frame) =
        val problem     = Json.decode[ArcProblem](test.input).getOrThrow
        val staticGrids = (problem.train ++ problem.test).flatMap(t => Seq(t.input, t.output)).map(Grid(_))

        val examplesMd =
            problem.train.zipWithIndex.map { case (task, idx) =>
                p"""
                ╔════════════════════════════════════════════════════════════════╗
                ║                 TRAINING EXAMPLE ${idx + 1}                    ║
                ╚════════════════════════════════════════════════════════════════╝
                
                INPUT (Example ${idx + 1}):
                ${Grid(task.input).toMarkdown}
                
                EXPECTED OUTPUT (Example ${idx + 1}):
                ${Grid(task.output).toMarkdown}
                
                ════════════════════════════════════════════════════════════════
            """
            }.mkString("\n")

        val problemMd =
            p"""
                LEARN THE TRANSFORMATION PATTERN FROM THESE EXAMPLES:
                
                $examplesMd
                
                ╔════════════════════════════════════════════════════════════════╗
                ║                        TEST TO SOLVE                           ║
                ╚════════════════════════════════════════════════════════════════╝
                
                APPLY THE PATTERN TO THIS INPUT:
                ${Grid(problem.test.head.input).toMarkdown}
                
                ────────────────────────────────────────────────────────────────
                IMPORTANT: Output a SINGLE 2D grid as outputGrid.
                Format: [[row1], [row2], ...] where each row is a list of integers.
                Example: [[0,1,0],[1,1,1],[0,1,0]] for a 3x3 grid.
                ────────────────────────────────────────────────────────────────
            """

        for
            timer <- Clock.stopwatch
            testAccuracy <-
                refine(mind, problem.train, problem.test.head)
                    .handle(
                        Prompt.init(problemMd).enable,
                        GridTool.run(staticGrids),
                        Async.timeout(1.hour),
                        Abort.recoverError[Throwable](e =>
                            Console.printLine("Error! " + mind.name + " " + e).andThen(0d)
                        )
                    )
            elapsed <- timer.elapsed
        yield
            val ok      = List.fill((50 * testAccuracy).ceil.toInt)("✅")
            val correct = if testAccuracy == 1 then List("🚀🚀🚀") else Nil
            val bar     = ok ::: List.fill(50 - ok.size)("☑️") ::: correct
            println(
                s"\n\n${Grid(problem.test.head.input).toMarkdown}\n\n🔥🔥🔥 SCORE 🔥🔥🔥 ${mind.name}: $testAccuracy \n${bar.mkString}\n\n\n\n\n"
            )
            Eval.Score(testAccuracy, elapsed)
        end for

    end apply

    private def refine(
        mind: Mind,
        examples: List[ArcProblem.Task],
        target: ArcProblem.Task
    )(using Frame): Double < AI =
        val expected = Grid(target.output)
        mind[Solution](Nil).map(s => Grid(s.outputGrid)).map { firstAttempt =>
            // Refinement loop
            Loop[(Grid, Int), Double, AI]((firstAttempt, 0)) { case (attempt, iter) =>
                val accuracy = gridAccuracy(attempt, expected)
                if accuracy == 1.0 || iter >= maxRefinements then
                    Loop.done(accuracy)
                else
                    val mask = createCorrectnessMask(attempt, expected)
                    mind[Solution](List(Input(
                        p"""
                            ╔════════════════════════════════════════════════════════════════╗
                            ║                     REFINEMENT FEEDBACK                        ║
                            ╚════════════════════════════════════════════════════════════════╝
                            
                            Your solution was ${(accuracy * 100).round}% correct.
                            
                            CORRECTNESS MASK (1=✓ correct, 0=✗ incorrect):
                            ${mask.toMarkdown(withAnalysis = false)}
                            
                            IMPORTANT REMINDERS:
                            - Cells marked with 0 are incorrect and need to be fixed
                            - Cells marked with 1 are already correct - keep them unchanged
                            
                            COMMON MISTAKES TO CHECK:
                            1. Are you using colors from the training examples instead of the test input?
                            2. Did you identify all the colors present in THIS test input?
                            3. Is the transformation rule being applied with THIS input's specific values?
                            
                            Remember: Each input has its own colors and elements. The transformation PATTERN 
                            is what you learn from training examples, but you must apply it using the 
                            specific colors and positions from THIS test input, not copy values from 
                            the training examples.
                            
                            Fix the cells marked with 0. Output the complete corrected grid.
                            Think carefully about what colors appear in THIS test input and how the 
                            learned transformation should use them.

                            This is the most challenging and critical part of your role. Getting to > 90%
                            accuracy is already working, this refinement mechanism is meant to cover the 
                            gap to 100%!
                        """
                    ))).map(s => Grid(s.outputGrid))
                        .map(solution => Loop.continue((solution, iter + 1)))
                end if
            }
        }
    end refine

    private def createCorrectnessMask(actual: Grid, expected: Grid): Grid =
        val minHeight = math.min(actual.height, expected.height)
        val minWidth  = math.min(actual.width, expected.width)

        val maskCells =
            (0 until minHeight).map { y =>
                (0 until minWidth).map { x =>
                    if actual.get(x, y) == expected.get(x, y) then 1 else 0
                }.toList ++
                    List.fill(math.max(0, expected.width - actual.width))(0)
            }.toList ++
                List.fill(math.max(0, expected.height - actual.height))(
                    List.fill(math.max(expected.width, actual.width))(0)
                )

        Grid(maskCells)
    end createCorrectnessMask
end ArcEval

object ArcEval:

    case class Solution(
        @doc("**Internal** reasoning regarding the generation")
        reasoning: Option[String],
        @doc("Output a single 2D grid. Example: [[0,1,0],[1,1,1],[0,1,0]] for a 3x3 grid. Do NOT use [[[...]]]")
        outputGrid: List[List[Int]]
    ) derives CanEqual, Json

    private def gridAccuracy(actual: Grid, expected: Grid): Double =
        if actual.cells.isEmpty || expected.cells.isEmpty then 0.0
        else if actual.cells.size != expected.cells.size then 0.0
        else
            val actualFlat   = actual.cells.flatten
            val expectedFlat = expected.cells.flatten

            if actualFlat.size != expectedFlat.size then 0.0
            else
                val matches = actualFlat.zip(expectedFlat).count { case (a, e) => a == e }
                matches.toDouble / actualFlat.size
            end if

end ArcEval
