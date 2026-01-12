package kyo.ren.evolution

import kyo.*
import kyo.MockGate
import kyo.Test
import kyo.ai.json.Json
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestCase
import kyo.ren.network.*

class EvalTest extends Test:

    def createMind(name: String)(using Frame): Mind < Sync =
        val gateState = Gate.State(
            prompt = "test prompt",
            reminder = "test reminder",
            temperature = Temperature.balanced,
            history = History.State(List.empty, 0)
        )
        val mindState = Mind.State(name, 1, List.empty, gateState)
        for
            gate  <- MockGate.init(MockGate.Response(output = "test output"))
            mutex <- Meter.initMutexUnscoped
        yield Mind(mindState, List.empty, gate, mutex)
        end for
    end createMind

    def createJudgeMind: Mind.State =
        Mind.State(
            name = "judge",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State("judge prompt", "judge reminder", Temperature.balanced, History.State(List.empty, 0))
        )

    "Score" - {
        "creates score with accuracy and latency" in {
            val score = Eval.Score(0.85, Duration.fromNanos(2))
            assert(score.accuracy == 0.85)
            assert(score.latency == Duration.fromNanos(2))
        }

        "handles zero accuracy" in {
            val score = Eval.Score(0.0, Duration.fromNanos(100))
            assert(score.accuracy == 0.0)
        }

        "handles perfect accuracy" in {
            val score = Eval.Score(1.0, Duration.fromNanos(50))
            assert(score.accuracy == 1.0)
        }
    }

    "State" - {
        "creates with no custom judge" in {
            val state = Eval.State()
            assert(state.customJudge.isEmpty)
        }

        "creates with custom judge" in {
            val customJudge = createJudgeMind
            val state       = Eval.State(customJudge = Some(customJudge))
            assert(state.customJudge.contains(customJudge))
        }

        "states with same values are equal" in {
            val judge  = createJudgeMind
            val state1 = Eval.State(customJudge = Some(judge))
            val state2 = Eval.State(customJudge = Some(judge))
            assert(state1 == state2)
        }

        "states with different judges are not equal" in {
            val judge1 = createJudgeMind
            val judge2 = createJudgeMind.copy(name = "different")
            val state1 = Eval.State(customJudge = Some(judge1))
            val state2 = Eval.State(customJudge = Some(judge2))
            assert(state1 != state2)
        }

        "empty states are equal" in {
            val state1 = Eval.State()
            val state2 = Eval.State()
            assert(state1 == state2)
        }
    }

    "Eval operations" - {
        "evaluates with mocked judge" in run {
            val state = Eval.State(customJudge = Some(createJudgeMind))

            val mockEval = new Eval:
                def apply(mind: Mind, input: TestCase)(using Frame) =
                    Clock.stopwatch.map { timer =>
                        timer.elapsed.map { latency =>
                            Eval.Score(0.9, latency)
                        }
                    }

            for
                testMind <- createMind("test-mind")
                score <- AI.run {
                    mockEval(testMind, TestCase("0", "input text", "expected output"))
                }
            yield
                assert(score.accuracy == 0.9)
                assert(score.latency.toNanos > 0)
            end for
        }

        "measures latency correctly" in run {
            val mockEval = new Eval:
                def apply(mind: Mind, test: TestCase)(using Frame) =
                    for
                        timer   <- Clock.stopwatch
                        _       <- Async.sleep(Duration.fromNanos(50))
                        _       <- mind[String](Input(test.input))
                        elapsed <- timer.elapsed
                    yield Eval.Score(0.5, elapsed)

            for
                gate  <- MockGate.init(MockGate.Response(output = "slow output"))
                mutex <- Meter.initMutexUnscoped
                slowMind = Mind(
                    Mind.State("slow", 1, List.empty, Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))),
                    List.empty,
                    gate,
                    mutex
                )
                score <- AI.run {
                    mockEval(slowMind, TestCase("0", "input", "expected"))
                }
            yield assert(score.latency.toNanos >= 50)
            end for
        }

        "creates eval from state with custom judge" in run {
            val judgeMindState = createJudgeMind
            val state          = Eval.State(customJudge = Some(judgeMindState))

            for
                eval <- Eval.init(state)
            yield assert(eval != null)
        }
    }

    "JudgeOutput" - {
        "includes reasoning and score" in {
            val output = Eval.JudgeOutput(Some("The output matches expectations"), 0.95)
            assert(output.reasoning.contains("The output matches expectations"))
            assert(output.score == 0.95)
        }

        "handles empty reasoning" in {
            val output = Eval.JudgeOutput(None, 0.5)
            assert(output.reasoning == None)
            assert(output.score == 0.5)
        }

        "serializes correctly" in {
            val output  = Eval.JudgeOutput(Some("reason"), 0.7)
            val encoded = Json.encode(output)
            assert(encoded.contains("\"reasoning\":\"reason\""))
            assert(encoded.contains("\"score\":0.7"))
        }
    }

    "JudgeInput" - {
        "contains all required fields" in {
            val input = Eval.JudgeInput(
                input = "test input",
                expected = "expected output",
                actual = "actual output"
            )
            assert(input.input == "test input")
            assert(input.expected == "expected output")
            assert(input.actual == "actual output")
        }

        "serializes correctly" in {
            val input   = Eval.JudgeInput("in", "exp", "act")
            val encoded = Json.encode(input)
            assert(encoded.contains("\"input\":\"in\""))
            assert(encoded.contains("\"expected\":\"exp\""))
            assert(encoded.contains("\"actual\":\"act\""))
        }

        "deserializes correctly" in {
            val original = Eval.JudgeInput("test", "expected", "actual")
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Eval.JudgeInput](encoded).getOrThrow
            assert(decoded == original)
        }
    }

end EvalTest
