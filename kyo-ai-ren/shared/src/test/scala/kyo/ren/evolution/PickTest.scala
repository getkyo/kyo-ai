package kyo.ren.evolution

import kyo.*
import kyo.Test
import kyo.ren.*

class PickTest extends Test:

    "should select based on weights" in run {
        val values  = List("a", "b", "c")
        val weights = Map("a" -> 1.0, "b" -> 2.0, "c" -> 1.0)

        for
            results <- Kyo.fill(100)(Pick(values, weights))
        yield
            val counts = results.groupBy(identity).map((k, v) => k -> v.size)
            assert(counts("b") > counts("a"))
            assert(counts("b") > counts("c"))
            assert(results.forall(values.contains))
        end for
    }

    "should handle single element" in run {
        val values = List("only")

        for
            result <- Pick(values, _ => 1.0)
        yield assert(result == "only")
    }

    "should handle zero weight appropriately" in run {
        val values  = List("a", "b", "c")
        val weights = Map("a" -> 0.0, "b" -> 1.0, "c" -> 0.0)

        for
            results <- Kyo.fill(50)(Pick(values, weights))
        yield assert(results.forall(_ == "b"))
    }

    "should fail with empty collection" in run {
        val values = List.empty[String]

        Abort.run[Throwable] {
            Pick(values, _ => 1.0)
        }.map { result =>
            assert(result.isFailure)
        }
    }

    "should fail with all negative weights" in run {
        val values = List("a", "b")

        Abort.run[Throwable] {
            Pick(values, _ => -1.0)
        }.map { result =>
            assert(result.isFailure)
        }
    }

    "should fail with all zero weights" in run {
        val values = List("a", "b", "c")

        Abort.run[Throwable] {
            Pick(values, _ => 0.0)
        }.map { result =>
            assert(result.isFailure)
        }
    }

    "should work with very small positive weights" in run {
        val values  = List("a", "b")
        val weights = Map("a" -> 0.0001, "b" -> 0.0002)

        for
            results <- Kyo.fill(100)(Pick(values, weights))
        yield
            val counts = results.groupBy(identity).map((k, v) => k -> v.size)
            assert(counts.contains("a"))
            assert(counts.contains("b"))
            assert(counts("b") > counts("a"))
        end for
    }

    "should handle floating point precision issues" in run {
        val values  = List.fill(100)("item")
        val weights = (i: String) => 0.01

        for
            results <- Kyo.fill(1000)(Pick(values.zipWithIndex.map(_._2.toString), i => weights("")))
        yield assert(results.toSet.size > 50)
    }

end PickTest
