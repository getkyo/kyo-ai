package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestCase

trait Fitness:
    def apply(mind: Mind, test: TestCase)(using Frame): Double < AI

object Fitness:

    case class State(
        eval: Eval.State = Eval.State(),
        @doc("Weight for execution speed in fitness calculation (0-1)")
        latencyWeight: Double = 0.3,
        @doc("Weight for computational cost in fitness calculation (0-1)")
        costWeight: Double = 0.1,
        @doc("Weight for correctness in fitness calculation (0-1)")
        accuracyWeight: Double = 0.7
    ) derives CanEqual, Json:
        require(latencyWeight >= 0, s"latencyWeight must be non-negative, got $latencyWeight")
        require(costWeight >= 0, s"costWeight must be non-negative, got $costWeight")
        require(accuracyWeight >= 0, s"accuracyWeight must be non-negative, got $accuracyWeight")
        require(latencyWeight + costWeight + accuracyWeight > 0, "At least one weight must be greater than zero")
    end State

    def init(state: State)(using Frame): Fitness < Sync =
        Eval.init(state.eval).map { eval =>
            init(eval, state.latencyWeight, state.costWeight, state.accuracyWeight)
        }

    def init(
        eval: Eval,
        latencyWeight: Double,
        costWeight: Double,
        accuracyWeight: Double
    )(using Frame): Fitness =
        val totalWeight = latencyWeight + costWeight + accuracyWeight
        require(totalWeight > 0, "Total weight must be positive")

        val normLatency  = latencyWeight / totalWeight
        val normCost     = costWeight / totalWeight
        val normAccuracy = accuracyWeight / totalWeight

        new Fitness:
            def apply(mind: Mind, test: TestCase)(using Frame) =
                for result <- eval(mind, test)
                yield
                    val latencyScore  = (1.0 / (1.0 + result.latency.toMillis / 1000.0)) * normLatency
                    val costScore     = 0.5 * normCost // TODO AI.stats with input/output token counts
                    val accuracyScore = result.accuracy * normAccuracy

                    latencyScore + costScore + accuracyScore
        end new
    end init

end Fitness
