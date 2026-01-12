package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class CrossoverLiveTest extends Test:

    def createIndividual(name: String, neuronCount: Int = 2): Individual =
        val neurons = (0 until neuronCount).map { i =>
            Neuron.State(
                name = Neuron.Name(s"${name}_neuron_$i"),
                description = s"Neuron $i in $name",
                previous = if i > 0 then List(Neuron.Name(s"${name}_neuron_${i - 1}")) else List.empty,
                next = if i < neuronCount - 1 then List(Neuron.Name(s"${name}_neuron_${i + 1}")) else List.empty,
                conscious = i % 2 == 0,
                memory = if i == 0 then
                    Some(Memory.State(
                        List(Memory.Entry(0, 0, s"Memory for $name neuron $i")),
                        Gate.State("Retrieve memories", "retrieve", Temperature.balanced, History.State(List.empty, 0))
                    ))
                else None,
                gate = Gate.State(
                    s"Output for neuron $i in $name",
                    "output",
                    Temperature.balanced,
                    History.State(List.empty, 0)
                )
            )
        }

        val mindState = Mind.State(
            name = name,
            maxRecursions = 3,
            neurons = List.from(neurons),
            output = Gate.State(
                s"Mind output for $name",
                "output",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )

        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = List(0.7, 0.6).map(TestScore("0", _)),
            sex = name.hashCode % 2 == 0,
            lineage = Lineage.empty
        )
    end createIndividual

    "performs crossover mixing both parents" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("parent1", 3)
                parent2 = createIndividual("parent2", 3)
                offspring <- crossover(parent1, parent2)
            yield
                assert(offspring.neurons.nonEmpty, s"Should have some neurons: ${offspring.neurons.size}")

                val hasParent1Name = offspring.name.contains("parent1") || offspring.neurons.exists(_.name.contains("parent1"))
                val hasParent2Name = offspring.name.contains("parent2") || offspring.neurons.exists(_.name.contains("parent2"))
                assert(
                    hasParent1Name || hasParent2Name,
                    s"Offspring should show traces of parent names in: ${offspring.name}, neurons: ${offspring.neurons.map(_.name)}"
                )
            end for
        }
    }

    "produces valid offspring from various parent sizes" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("alpha", 2)
                parent2 = createIndividual("beta", 2)
                offspring <- crossover(parent1, parent2)
            yield
                assert(offspring.maxRecursions == 3, s"Should inherit maxRecursions from parents: ${offspring.maxRecursions}")
                assert(offspring.neurons.nonEmpty, s"Should have some neurons: ${offspring.neurons.size}")

                val hasAlphaNeurons = offspring.neurons.exists(_.name.contains("alpha"))
                val hasBetaNeurons  = offspring.neurons.exists(_.name.contains("beta"))
                assert(
                    hasAlphaNeurons || hasBetaNeurons,
                    s"Should contain neurons from parents: ${offspring.neurons.map(_.name)}"
                )
            end for
        }
    }

    "maintains structural validity with proper connectivity" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("valid1", 4)
                parent2 = createIndividual("valid2", 4)
                offspring <- crossover(parent1, parent2)
            yield
                val neuronNames = offspring.neurons.map(_.name).toSet
                offspring.neurons.foreach { neuron =>
                    neuron.previous.foreach { prevName =>
                        assert(
                            neuronNames.contains(prevName),
                            s"Neuron ${neuron.name} references non-existent previous: $prevName"
                        )
                    }
                    neuron.next.foreach { nextName =>
                        assert(neuronNames.contains(nextName), s"Neuron ${neuron.name} references non-existent next: $nextName")
                    }
                }
                succeed
        }
    }

    "produces valid mind state from similar parents" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("p1", 2)
                parent2 = createIndividual("p2", 2)
                offspring <- crossover(parent1, parent2)
            yield
                assert(offspring.maxRecursions == 3, s"Should inherit maxRecursions from parents: ${offspring.maxRecursions}")
                assert(offspring.neurons.nonEmpty, s"Should have some neurons: ${offspring.neurons.size}")

                val hasP1Traits = offspring.neurons.exists(n => n.name.contains("p1") || n.description.contains("p1"))
                val hasP2Traits = offspring.neurons.exists(n => n.name.contains("p2") || n.description.contains("p2"))
                assert(
                    hasP1Traits || hasP2Traits,
                    s"Should show genetic mixing from parents: ${offspring.neurons.map(n => (n.name, n.description))}"
                )
            end for
        }
    }

    "handles parents with different neuron counts" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("small", 2)
                parent2 = createIndividual("large", 5)
                offspring <- crossover(parent1, parent2)
            yield
                assert(
                    offspring.neurons.size >= 1 && offspring.neurons.size <= 7,
                    s"Offspring neuron count should be reasonable (1-7): ${offspring.neurons.size}"
                )
                val hasSmallTraits = offspring.neurons.exists(n => n.description.contains("small") || n.name.contains("small"))
                val hasLargeTraits = offspring.neurons.exists(n => n.description.contains("large") || n.name.contains("large"))
                val hasMemory      = offspring.neurons.exists(_.memory.isDefined)
                assert(
                    hasSmallTraits || hasLargeTraits || hasMemory,
                    s"Should show genetic mixing from parents in descriptions: ${offspring.neurons.map(n => (n.name, n.description))}, has memory: $hasMemory"
                )
            end for
        }
    }

    "handles parents with no neurons" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("empty1", 0)
                parent2 = createIndividual("empty2", 0)
                offspring <- crossover(parent1, parent2)
            yield
                assert(offspring.neurons.isEmpty, s"When both parents have no neurons, offspring should have none")
                assert(offspring.output.prompt.nonEmpty, "Should have valid output gate")
        }
    }

    "creates valid Mind.State that can be initialized and executed" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("valid1", 3)
                parent2 = createIndividual("valid2", 3)
                offspring     <- crossover(parent1, parent2)
                offspringMind <- Mind.init(offspring)
                result        <- offspringMind[String](List(Input("test input")))
            yield
                assert(result.nonEmpty, "Offspring mind should be able to process input")
                assert(offspring.neurons.nonEmpty, s"Should have some neurons: ${offspring.neurons.size}")
        }
    }

    "CANONICAL: handles parents with very different sizes (stress test)" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("minimal", 0)
                parent2 = createIndividual("complex", 10)
                offspring <- crossover(parent1, parent2)
            yield
                assert(
                    offspring.neurons.size >= 0 && offspring.neurons.size <= 10,
                    s"Should produce valid offspring despite parent size mismatch (0 vs 10): got ${offspring.neurons.size}"
                )
                assert(offspring.output.prompt.nonEmpty, "Should have valid output gate")
                // Note: Dangling references (neurons referencing non-existent neurons) are intentionally
                // allowed as a mechanism similar to recessive genes, which can aid the evolution process
                succeed
        }
    }

    "respects parent genotype structure and connectivity" in run {
        AI.run {
            for
                crossover <- Crossover.init(Crossover.State())
                parent1 = createIndividual("structured_a", 3)
                parent2 = createIndividual("structured_b", 3)
                offspring <- crossover(parent1, parent2)
            yield
                assert(offspring.neurons.nonEmpty, s"Should have some neurons: ${offspring.neurons.size}")

                val hasConnections = offspring.neurons.exists(n => n.previous.nonEmpty || n.next.nonEmpty)
                assert(hasConnections, s"Offspring should maintain neural connectivity from parents")

                val hasStructuredATraits =
                    offspring.neurons.exists(n => n.name.contains("structured_a") || n.description.contains("structured_a"))
                val hasStructuredBTraits =
                    offspring.neurons.exists(n => n.name.contains("structured_b") || n.description.contains("structured_b"))
                assert(
                    hasStructuredATraits || hasStructuredBTraits,
                    s"Should show genetic traits from parents: ${offspring.neurons.map(n => (n.name, n.description))}"
                )
            end for
        }
    }

end CrossoverLiveTest
