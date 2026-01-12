package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestCase
import kyo.ren.network.Input

trait Eval:
    def apply(mind: Mind, test: TestCase)(using Frame): Eval.Score < AI

object Eval:

    case class Score(
        accuracy: Double,
        latency: Duration
    ) derives CanEqual, Json

    case class State(
        customJudge: Option[Mind.State] = None
    ) derives CanEqual, Json

    def init(state: State)(using Frame): Eval < Sync =
        Mind.loadDefault("evolution", "eval", "judge", state.customJudge).map { judge =>
            new Eval:
                def apply(mind: Mind, test: TestCase)(using Frame) =
                    for
                        timer     <- Clock.stopwatch
                        output    <- mind[String](List(Input(List(test.input))))
                        elapsed   <- timer.elapsed
                        judgeMind <- Mind.init(judge)
                        result <- judgeMind[JudgeOutput](Input(List(
                            JudgeInput(
                                input = test.input,
                                expected = test.expected,
                                actual = output
                            )
                        )))
                    yield Score(result.score, elapsed)
        }
    end init

    case class JudgeInput(
        input: String,
        expected: String,
        actual: String
    ) derives CanEqual, Json

    case class JudgeOutput(
        @doc("**Internal** reasoning regarding the generation")
        reasoning: Option[String] = None,
        score: Double
    ) derives CanEqual, Json

end Eval
