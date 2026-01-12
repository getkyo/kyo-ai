package kyo.ren.evolution

import kyo.*
import kyo.ai.json.Json
import kyo.ren.*
import scala.annotation.tailrec
import scala.collection.mutable

object Pick:
    def apply[A](values: List[A], weight: A => Double)(using Frame): A < Sync =
        require(values.nonEmpty, "Cannot pick from empty collection")

        Random.nextDouble.map { r =>
            val weighted = values.map(v => (v, weight(v)))
            val weights  = weighted.map(_._2)

            require(weights.forall(_ >= 0), "All weights must be non-negative")

            val totalWeight = weights.sum
            require(totalWeight > 0, "Total weight must be positive")

            val sorted = weighted.sortBy(-_._2)
            val target = r * totalWeight

            @tailrec
            def loop(remaining: List[(A, Double)], sum: Double): A =
                remaining match
                    case Nil => sorted.last._1
                    case (value, w) :: tail =>
                        val newSum = sum + w
                        if newSum >= target then value
                        else loop(tail, newSum)

            loop(sorted.toList, 0.0)
        }
    end apply
end Pick
