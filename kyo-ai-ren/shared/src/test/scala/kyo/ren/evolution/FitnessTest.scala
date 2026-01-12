package kyo.ren.evolution

import kyo.*
import kyo.MockGate
import kyo.Test
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestCase
import kyo.ren.network.*

class FitnessTest extends Test:

    def createMind(name: String)(using Frame): Mind < Sync =
        val gateState = Gate.State(
            prompt = "test prompt",
            reminder = "test reminder",
            temperature = Temperature.balanced,
            history = History.State(List.empty, 0)
        )
        val mindState = Mind.State(name, 1, List.empty, gateState)
        for
            gate  <- MockGate.init(MockGate.Response(output = "output"))
            mutex <- Meter.initMutexUnscoped
        yield Mind(mindState, List.empty, gate, mutex)
        end for
    end createMind

    def createMockEval(accuracy: Double, latencyMillis: Long): Eval =
        new Eval:
            def apply(mind: Mind, test: TestCase)(using Frame) =
                Eval.Score(accuracy, latencyMillis.millis)

    "State validation" - {
        "accepts valid fitness state" in {
            val state = Fitness.State(
                eval = Eval.State(),
                latencyWeight = 0.2,
                costWeight = 0.1,
                accuracyWeight = 0.7
            )
            assert(state.latencyWeight == 0.2)
            assert(state.costWeight == 0.1)
            assert(state.accuracyWeight == 0.7)
        }

        "requires non-negative latencyWeight" in {
            val result = scala.util.Try {
                Fitness.State(
                    latencyWeight = -0.1,
                    costWeight = 0.1,
                    accuracyWeight = 0.7
                )
            }
            assert(result.isFailure)
        }

        "requires non-negative costWeight" in {
            val result = scala.util.Try {
                Fitness.State(
                    latencyWeight = 0.2,
                    costWeight = -0.1,
                    accuracyWeight = 0.7
                )
            }
            assert(result.isFailure)
        }

        "requires non-negative accuracyWeight" in {
            val result = scala.util.Try {
                Fitness.State(
                    latencyWeight = 0.2,
                    costWeight = 0.1,
                    accuracyWeight = -0.1
                )
            }
            assert(result.isFailure)
        }

        "requires at least one positive weight" in {
            val result = scala.util.Try {
                Fitness.State(
                    latencyWeight = 0.0,
                    costWeight = 0.0,
                    accuracyWeight = 0.0
                )
            }
            assert(result.isFailure)
        }

        "accepts all zero weights except one" in {
            val state1 = Fitness.State(latencyWeight = 1.0, costWeight = 0.0, accuracyWeight = 0.0)
            assert(state1.latencyWeight == 1.0)

            val state2 = Fitness.State(latencyWeight = 0.0, costWeight = 1.0, accuracyWeight = 0.0)
            assert(state2.costWeight == 1.0)

            val state3 = Fitness.State(latencyWeight = 0.0, costWeight = 0.0, accuracyWeight = 1.0)
            assert(state3.accuracyWeight == 1.0)
        }

        "accepts equal weights" in {
            val state = Fitness.State(
                latencyWeight = 0.33,
                costWeight = 0.33,
                accuracyWeight = 0.34
            )
            assert(state.latencyWeight == 0.33)
            assert(state.costWeight == 0.33)
            assert(state.accuracyWeight == 0.34)
        }
    }

    "Fitness initialization" - {
        "normalizes weights correctly" in run {
            val eval    = createMockEval(0.8, 100)
            val fitness = Fitness.init(eval, 2.0, 1.0, 3.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(result >= 0.0 && result <= 1.0)
            end for
        }

        "requires positive total weight" in {
            val eval = createMockEval(0.5, 100)
            val result = scala.util.Try {
                Fitness.init(eval, 0.0, 0.0, 0.0)
            }
            assert(result.isFailure)
        }

        "handles single weight dimension" in run {
            val eval    = createMockEval(0.9, 100)
            val fitness = Fitness.init(eval, 0.0, 0.0, 1.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(result == 0.9)
            end for
        }
    }

    "Fitness scoring" - {
        "calculates weighted score correctly" in run {
            val eval    = createMockEval(0.8, 1000)
            val fitness = Fitness.init(eval, 0.2, 0.1, 0.7)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield
                val expectedLatencyScore  = (1.0 / (1.0 + 1000.0 / 1000.0)) * 0.2
                val expectedCostScore     = 0.5 * 0.1
                val expectedAccuracyScore = 0.8 * 0.7
                val expectedTotal         = expectedLatencyScore + expectedCostScore + expectedAccuracyScore

                assert(math.abs(score - expectedTotal) < 0.001)
                assert(math.abs(score - 0.71) < 0.001)
            end for
        }

        "handles perfect accuracy" in run {
            val eval    = createMockEval(1.0, 100)
            val fitness = Fitness.init(eval, 0.0, 0.0, 1.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(score == 1.0)
            end for
        }

        "handles zero accuracy" in run {
            val eval    = createMockEval(0.0, 100)
            val fitness = Fitness.init(eval, 0.0, 0.0, 1.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(score == 0.0)
            end for
        }

        "penalizes high latency" in run {
            val fastEval = createMockEval(0.5, 100)
            val slowEval = createMockEval(0.5, 10000)

            val fitnessFast = Fitness.init(fastEval, 1.0, 0.0, 0.0)
            val fitnessSlow = Fitness.init(slowEval, 1.0, 0.0, 0.0)

            for
                mind <- createMind("test")
                (fastScore, slowScore) <- AI.run {
                    for
                        fastScore <- fitnessFast(mind, TestCase("0", "input", "expected"))
                        slowScore <- fitnessSlow(mind, TestCase("0", "input", "expected"))
                    yield (fastScore, slowScore)
                }
            yield
                val expectedFast = 1.0 / (1.0 + 100.0 / 1000.0)
                val expectedSlow = 1.0 / (1.0 + 10000.0 / 1000.0)
                assert(fastScore > slowScore)
                assert(math.abs(fastScore - expectedFast) < 0.001)
                assert(math.abs(slowScore - expectedSlow) < 0.001)
            end for
        }

        "includes fixed cost component" in run {
            val eval    = createMockEval(0.0, Long.MaxValue)
            val fitness = Fitness.init(eval, 0.0, 1.0, 0.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(score == 0.5)
            end for
        }

        "balances multiple factors" in run {
            val eval    = createMockEval(0.6, 2000)
            val fitness = Fitness.init(eval, 1.0, 1.0, 1.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield
                val normLatency  = 1.0 / 3.0
                val normCost     = 1.0 / 3.0
                val normAccuracy = 1.0 / 3.0

                val latencyScore  = (1.0 / (1.0 + 2000.0 / 1000.0)) * normLatency
                val costScore     = 0.5 * normCost
                val accuracyScore = 0.6 * normAccuracy
                val expectedTotal = latencyScore + costScore + accuracyScore

                assert(math.abs(score - expectedTotal) < 0.001)
            end for
        }

        "handles zero latency" in run {
            val eval    = createMockEval(0.5, 0)
            val fitness = Fitness.init(eval, 1.0, 0.0, 0.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield assert(score == 1.0)
            end for
        }

        "handles very high latency" in run {
            val eval    = createMockEval(0.5, 1000000)
            val fitness = Fitness.init(eval, 1.0, 0.0, 0.0)

            for
                mind <- createMind("test")
                score <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected"))
                }
            yield
                val expected = 1.0 / (1.0 + 1000000.0 / 1000.0)
                assert(math.abs(score - expected) < 0.001)
                assert(score < 0.01)
            end for
        }
    }

    "State equality" - {
        "states with same values are equal" in {
            val state1 = Fitness.State(
                eval = Eval.State(),
                latencyWeight = 0.2,
                costWeight = 0.3,
                accuracyWeight = 0.5
            )
            val state2 = Fitness.State(
                eval = Eval.State(),
                latencyWeight = 0.2,
                costWeight = 0.3,
                accuracyWeight = 0.5
            )
            assert(state1 == state2)
        }

        "states with different weights are not equal" in {
            val state1 = Fitness.State(latencyWeight = 0.2, costWeight = 0.3, accuracyWeight = 0.5)
            val state2 = Fitness.State(latencyWeight = 0.3, costWeight = 0.2, accuracyWeight = 0.5)
            assert(state1 != state2)
        }

        "states with different eval states are not equal" in {
            val evalState1 = Eval.State()
            val evalState2 = Eval.State(customJudge =
                Some(Mind.State(
                    "judge",
                    1,
                    List.empty,
                    Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                ))
            )

            val state1 = Fitness.State(eval = evalState1)
            val state2 = Fitness.State(eval = evalState2)
            assert(state1 != state2)
        }
    }

    "Component breakdown" - {
        "accuracy component contributes independently" in run {
            val evalLow  = createMockEval(0.3, 1000)
            val evalHigh = createMockEval(0.9, 1000)

            val fitnessLow  = Fitness.init(evalLow, 0.0, 0.0, 1.0)
            val fitnessHigh = Fitness.init(evalHigh, 0.0, 0.0, 1.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreLow  <- fitnessLow(mind, TestCase("0", "input", "expected"))
                        scoreHigh <- fitnessHigh(mind, TestCase("0", "input", "expected"))
                    yield
                        assert(scoreLow == 0.3)
                        assert(scoreHigh == 0.9)
                        assert(scoreHigh > scoreLow)
                    end for
                }
            yield result
            end for
        }

        "latency component contributes independently" in run {
            val evalFast = createMockEval(0.5, 500)
            val evalSlow = createMockEval(0.5, 5000)

            val fitnessFast = Fitness.init(evalFast, 1.0, 0.0, 0.0)
            val fitnessSlow = Fitness.init(evalSlow, 1.0, 0.0, 0.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreFast <- fitnessFast(mind, TestCase("0", "input", "expected"))
                        scoreSlow <- fitnessSlow(mind, TestCase("0", "input", "expected"))
                    yield
                        val expectedFast = 1.0 / (1.0 + 500.0 / 1000.0)
                        val expectedSlow = 1.0 / (1.0 + 5000.0 / 1000.0)
                        assert(math.abs(scoreFast - expectedFast) < 0.001)
                        assert(math.abs(scoreSlow - expectedSlow) < 0.001)
                        assert(scoreFast > scoreSlow)
                    end for
                }
            yield result
            end for
        }

        "cost component contributes independently" in run {
            val eval = createMockEval(0.5, 1000)

            val fitnessNoCost   = Fitness.init(eval, 0.0, 0.0, 1.0)
            val fitnessWithCost = Fitness.init(eval, 0.0, 1.0, 0.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreNoCost   <- fitnessNoCost(mind, TestCase("0", "input", "expected"))
                        scoreWithCost <- fitnessWithCost(mind, TestCase("0", "input", "expected"))
                    yield
                        assert(scoreNoCost == 0.5)
                        assert(scoreWithCost == 0.5)
                    end for
                }
            yield result
            end for
        }

        "all three components sum correctly" in run {
            val eval    = createMockEval(0.6, 2000)
            val fitness = Fitness.init(eval, 0.3, 0.1, 0.6)

            for
                mind <- createMind("test")
                result <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected")).map { total =>
                        val latencyScore  = (1.0 / (1.0 + 2000.0 / 1000.0)) * 0.3
                        val costScore     = 0.5 * 0.1
                        val accuracyScore = 0.6 * 0.6

                        val expectedTotal = latencyScore + costScore + accuracyScore

                        assert(math.abs(total - expectedTotal) < 0.001)
                        assert(total >= 0.0 && total <= 1.0)
                    }
                }
            yield result
            end for
        }

        "zero weight eliminates component contribution" in run {
            val eval = createMockEval(0.0, Long.MaxValue)

            val fitnessAccuracyOnly = Fitness.init(eval, 0.0, 0.0, 1.0)
            val fitnessLatencyOnly  = Fitness.init(eval, 1.0, 0.0, 0.0)
            val fitnessCostOnly     = Fitness.init(eval, 0.0, 1.0, 0.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreAccuracy <- fitnessAccuracyOnly(mind, TestCase("0", "input", "expected"))
                        scoreLatency  <- fitnessLatencyOnly(mind, TestCase("0", "input", "expected"))
                        scoreCost     <- fitnessCostOnly(mind, TestCase("0", "input", "expected"))
                    yield
                        assert(scoreAccuracy == 0.0)
                        assert(scoreLatency < 0.01)
                        assert(scoreCost == 0.5)
                    end for
                }
            yield result
            end for
        }

        "component weights affect relative contribution" in run {
            val eval = createMockEval(0.8, 1000)

            val fitnessAccuracyHeavy = Fitness.init(eval, 0.1, 0.1, 0.8)
            val fitnessBalanced      = Fitness.init(eval, 0.33, 0.34, 0.33)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreHeavy    <- fitnessAccuracyHeavy(mind, TestCase("0", "input", "expected"))
                        scoreBalanced <- fitnessBalanced(mind, TestCase("0", "input", "expected"))
                    yield assert(scoreHeavy > scoreBalanced)
                    end for
                }
            yield result
            end for
        }

        "changing single component changes total score proportionally" in run {
            val evalLowAccuracy  = createMockEval(0.2, 1000)
            val evalHighAccuracy = createMockEval(0.8, 1000)

            val fitnessLow  = Fitness.init(evalLowAccuracy, 0.2, 0.2, 0.6)
            val fitnessHigh = Fitness.init(evalHighAccuracy, 0.2, 0.2, 0.6)

            for
                mind <- createMind("test")
                result <- AI.run {
                    for
                        scoreLow  <- fitnessLow(mind, TestCase("0", "input", "expected"))
                        scoreHigh <- fitnessHigh(mind, TestCase("0", "input", "expected"))
                    yield
                        val accuracyContribLow  = 0.2 * 0.6
                        val accuracyContribHigh = 0.8 * 0.6
                        val difference          = scoreHigh - scoreLow
                        val expectedDifference  = accuracyContribHigh - accuracyContribLow

                        assert(math.abs(difference - expectedDifference) < 0.001)
                    end for
                }
            yield result
            end for
        }

        "maximum possible score is 1.0" in run {
            val evalPerfect = createMockEval(1.0, 0)
            val fitness     = Fitness.init(evalPerfect, 1.0, 1.0, 1.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected")).map { score =>
                        val latencyScore  = 1.0 * (1.0 / 3.0)
                        val costScore     = 0.5 * (1.0 / 3.0)
                        val accuracyScore = 1.0 * (1.0 / 3.0)
                        val expectedTotal = latencyScore + costScore + accuracyScore

                        assert(math.abs(score - expectedTotal) < 0.001)
                        assert(score < 1.0)
                    }
                }
            yield result
            end for
        }

        "minimum possible score is 0.0 with zero accuracy" in run {
            val evalZero = createMockEval(0.0, Long.MaxValue)
            val fitness  = Fitness.init(evalZero, 0.0, 0.0, 1.0)

            for
                mind <- createMind("test")
                result <- AI.run {
                    fitness(mind, TestCase("0", "input", "expected")).map { score =>
                        assert(score == 0.0)
                    }
                }
            yield result
            end for
        }
    }

end FitnessTest
