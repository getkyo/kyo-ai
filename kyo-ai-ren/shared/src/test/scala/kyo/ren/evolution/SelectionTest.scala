package kyo.ren.evolution

import kyo.*
import kyo.Test
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class SelectionTest extends Test:

    def createIndividual(name: String, fitness: List[Double]): Individual =
        val gateState = Gate.State(
            prompt = "test prompt",
            reminder = "test reminder",
            temperature = Temperature.balanced,
            history = History.State(List.empty, 0)
        )
        val mindState = Mind.State(name, 1, List.empty, gateState)
        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = fitness.map(TestScore("0", _)),
            sex = false,
            lineage = Lineage.empty
        )
    end createIndividual

    "Fitness" - {
        "should select top individuals by total fitness" in run {
            val individuals = List(
                createIndividual("low", List(0.1, 0.1, 0.1)),
                createIndividual("high", List(0.9, 0.9, 0.9)),
                createIndividual("mid", List(0.5, 0.5, 0.5))
            )

            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(individuals, 2)
            yield
                assert(selected.size == 2)
                assert(selected(0).genotype.name == "high")
                assert(selected(1).genotype.name == "mid")
            end for
        }

        "should handle empty population" in run {
            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(List.empty, 5)
            yield assert(selected.isEmpty)
        }

        "should handle request for zero individuals" in run {
            val individuals = List(
                createIndividual("a", List(0.5)),
                createIndividual("b", List(0.6))
            )

            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(individuals, 0)
            yield assert(selected.isEmpty)
            end for
        }

        "should return all when request exceeds population" in run {
            val individuals = List(
                createIndividual("a", List(0.7)),
                createIndividual("b", List(0.3))
            )

            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(individuals, 10)
            yield assert(selected == individuals.sortBy(-_.fitness.map(_.score).sum))
            end for
        }

        "should handle negative count" in run {
            val individuals = List(createIndividual("a", List(0.5)))

            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(individuals, -1)
            yield assert(selected.isEmpty)
            end for
        }

        "should maintain order for equal fitness" in run {
            val individuals = List(
                createIndividual("first", List(0.5, 0.5)),
                createIndividual("second", List(0.5, 0.5)),
                createIndividual("third", List(0.5, 0.5))
            )

            val selector = Selection.init(Selection.State.Fitness)
            for
                selected <- selector.sample(individuals, 3)
            yield assert(selected.map(_.genotype.name) == List("first", "second", "third"))
            end for
        }
    }

    "Pareto" - {
        "should identify non-dominated front" in run {
            val individuals = List(
                createIndividual("dominated", List(0.5, 0.5, 0.5)),
                createIndividual("pareto1", List(0.9, 0.3, 0.6)),
                createIndividual("pareto2", List(0.3, 0.9, 0.6))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 2)
            yield
                assert(selected.size == 2)
                assert(selected.map(_.genotype.name).toSet == Set("pareto1", "pareto2"))
            end for
        }

        "should handle single individual" in run {
            val individual = createIndividual("solo", List(0.5, 0.5))

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(List(individual), 1)
            yield assert(selected == List(individual))
            end for
        }

        "should handle all dominated by one" in run {
            val individuals = List(
                createIndividual("best", List(0.9, 0.9, 0.9)),
                createIndividual("worse1", List(0.3, 0.3, 0.3)),
                createIndividual("worse2", List(0.5, 0.5, 0.5))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 1)
            yield
                assert(selected.size == 1)
                assert(selected(0).genotype.name == "best")
            end for
        }

        "should add by fitness when front smaller than requested" in run {
            val individuals = List(
                createIndividual("pareto", List(1.0, 0.0)),
                createIndividual("mid", List(0.5, 0.4)),
                createIndividual("low", List(0.3, 0.2))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 3)
            yield
                assert(selected.size == 3)
                assert(selected(0).genotype.name == "pareto")
                assert(selected(1).genotype.name == "mid")
                assert(selected(2).genotype.name == "low")
            end for
        }

        "should handle empty population" in run {
            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(List.empty[Individual], 5)
            yield assert(selected.isEmpty)
        }

        "should handle zero request" in run {
            val individuals = List(createIndividual("a", List(0.5)))

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 0)
            yield assert(selected.isEmpty)
            end for
        }

        "should prioritize diverse coverage in Pareto front" in run {
            val individuals = List(
                createIndividual("test1_best", List(1.0, 0.0, 0.5)),
                createIndividual("test2_best", List(0.0, 1.0, 0.5)),
                createIndividual("test3_best", List(0.5, 0.5, 1.0)),
                createIndividual("mediocre", List(0.4, 0.4, 0.4))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 2)
            yield
                assert(selected.size == 2)
                val names = selected.map(_.genotype.name)
                assert(names.forall(_.endsWith("_best")))
            end for
        }

        "handles ties in test scores correctly" in run {
            val individuals = List(
                createIndividual("a", List(0.8, 0.5)),
                createIndividual("b", List(0.8, 0.5)), // Identical to 'a'
                createIndividual("c", List(0.7, 0.6))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 3)
            yield
            // All should be selected since none strictly dominates others
            assert(selected.size == 3)
            end for
        }

        "single test dimension behaves like fitness selection" in run {
            val individuals = List(
                createIndividual("low", List(0.2)),
                createIndividual("high", List(0.9)),
                createIndividual("mid", List(0.5))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 2)
            yield
                // With single dimension, Pareto front = highest fitness
                assert(selected.size == 2)
                assert(selected(0).genotype.name == "high")
                assert(selected(1).genotype.name == "mid")
            end for
        }

        "all individuals on Pareto front when no domination exists" in run {
            // Trade-off scenario: each excels in different dimension
            val individuals = List(
                createIndividual("specialist1", List(1.0, 0.0, 0.0)),
                createIndividual("specialist2", List(0.0, 1.0, 0.0)),
                createIndividual("specialist3", List(0.0, 0.0, 1.0))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 2)
            yield
            // All 3 are on Pareto front, should select 2 by fitness fallback
            assert(selected.size == 2)
            end for
        }

        "frontier includes individual dominating on single test only" in run {
            val individuals = List(
                createIndividual("balanced", List(0.7, 0.7, 0.7)),
                createIndividual("oneStrength", List(0.9, 0.5, 0.5))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 2)
            yield
            // oneStrength dominates on test 0, balanced on tests 1&2
            // Both should be on frontier
            assert(selected.size == 2)
            end for
        }

        "excludes clearly dominated individuals from frontier" in run {
            val individuals = List(
                createIndividual("dominant", List(0.9, 0.9)),
                createIndividual("dominated1", List(0.5, 0.5)),
                createIndividual("dominated2", List(0.3, 0.3))
            )

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(individuals, 1)
            yield
                // Only dominant should be on frontier
                assert(selected.size == 1)
                assert(selected(0).genotype.name == "dominant")
            end for
        }
    }

    "Pareto domination logic" - {
        "A dominates B when strictly better on all dimensions" in run {
            val better = createIndividual("better", List(0.9, 0.9, 0.9))
            val worse  = createIndividual("worse", List(0.5, 0.5, 0.5))

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(List(better, worse), 1)
            yield
                assert(selected.size == 1)
                assert(selected(0) == better)
            end for
        }

        "A does not dominate B when worse on any dimension" in run {
            val a = createIndividual("a", List(0.9, 0.3))
            val b = createIndividual("b", List(0.3, 0.9))

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(List(a, b), 2)
            yield
            // Both on frontier
            assert(selected.size == 2)
            end for
        }

        "identical fitness means neither dominates" in run {
            val a = createIndividual("a", List(0.5, 0.5))
            val b = createIndividual("b", List(0.5, 0.5))

            val selector = Selection.init(Selection.State.Pareto)
            for
                selected <- selector.sample(List(a, b), 2)
            yield assert(selected.size == 2)
            end for
        }
    }

end SelectionTest
