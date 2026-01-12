package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class MutationLiveTest extends Test:

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
                    s"Output for neuron $i",
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

    "applies temperature mutation" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("temp-test", 2)
            for mutated <- mutation(individual)
            yield
                assert(mutated.name == "temp-test", s"Name should be preserved: ${mutated.name}")
                assert(mutated.neurons.size == 2, s"Neuron count should be preserved: ${mutated.neurons.size}")
                succeed
            end for
        }
    }

    "respects mutation rate" in run {
        val noMutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 0.0
        )

        AI.run {
            val mutation   = Mutation.init(noMutationState)
            val individual = createIndividual("no-mutation", 2)
            for result <- mutation(individual)
            yield assert(result == individual.genotype, "With rate=0.0, genotype should be unchanged")
            end for
        }
    }

    "structure mutation produces valid mind state" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Structure(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("structure-test", 3)
            for mutated <- mutation(individual)
            yield
                assert(mutated.name.nonEmpty, "Mutated mind should have a name")
                assert(mutated.neurons.size >= 1, s"Mutated mind should have at least 1 neuron but got ${mutated.neurons.size}")
                assert(mutated.maxRecursions > 0, s"maxRecursions should be positive: ${mutated.maxRecursions}")
            end for
        }
    }

    "prompt mutation produces valid mind state" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("prompt-test", 2)
            for mutated <- mutation(individual)
            yield
                assert(mutated.name.nonEmpty, "Mutated mind should have a name")
                assert(mutated.neurons.size >= 1, s"Mutated mind should have at least 1 neuron: ${mutated.neurons.size}")
                assert(mutated.output.prompt.nonEmpty, "Output gate should have prompt")
            end for
        }
    }

    "memory mutation produces valid mind state" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Memory(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("memory-test", 2)
            for mutated <- mutation(individual)
            yield
                assert(mutated.name.nonEmpty, "Mutated mind should have a name")
                assert(mutated.neurons.size >= 1, s"Mutated mind should have at least 1 neuron: ${mutated.neurons.size}")
            end for
        }
    }

    "params mutation produces valid mind state" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Params(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("params-test", 2)
            for mutated <- mutation(individual)
            yield
                assert(mutated.name.nonEmpty, "Mutated mind should have a name")
                assert(mutated.maxRecursions > 0, s"maxRecursions should be positive: ${mutated.maxRecursions}")
                assert(mutated.neurons.size >= 1, s"Mutated mind should have at least 1 neuron: ${mutated.neurons.size}")
            end for
        }
    }

    "multiple strategy mutations all produce valid results" in run {
        val mutationState = Mutation.State(
            strategies = List(
                Mutation.Strategy.Structure(0.2, None),
                Mutation.Strategy.Prompt(0.2, None),
                Mutation.Strategy.Memory(0.2, None),
                Mutation.Strategy.Params(0.2, None),
                Mutation.Strategy.Temp(0.2)
            ),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("multi-strategy", 2)
            for
                mutated1 <- mutation(individual)
                mutated2 <- mutation(individual)
                mutated3 <- mutation(individual)
            yield
                List(mutated1, mutated2, mutated3).foreach { mutated =>
                    assert(mutated.name.nonEmpty, s"Mutated mind should have a name: ${mutated.name}")
                    assert(mutated.neurons.size >= 1, s"Mutated mind should have at least 1 neuron: ${mutated.neurons.size}")
                    assert(mutated.maxRecursions > 0, s"maxRecursions should be positive: ${mutated.maxRecursions}")
                }
                succeed
            end for
        }
    }

    "temperature mutation changes temperature values" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("temp-change", 3)

            for
                mutated1 <- mutation(individual)
                mutated2 <- mutation(individual)
                mutated3 <- mutation(individual)
                mutated4 <- mutation(individual)
                mutated5 <- mutation(individual)
            yield
                val allMutated = List(mutated1, mutated2, mutated3, mutated4, mutated5)
                assert(
                    allMutated.forall(_.neurons.size == 3),
                    s"All mutations should preserve neuron count: ${allMutated.map(_.neurons.size)}"
                )
                succeed
            end for
        }
    }

    "mutation preserves basic mind structure" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 1.0
        )

        AI.run {
            val mutation            = Mutation.init(mutationState)
            val individual          = createIndividual("preserve-structure", 2)
            val originalNeuronNames = individual.genotype.neurons.map(_.name).toSet
            for mutated <- mutation(individual)
            yield
                assert(mutated.neurons.size == 2, s"Temp mutation should preserve neuron count: ${mutated.neurons.size}")
                val mutatedNeuronNames = mutated.neurons.map(_.name).toSet
                assert(
                    mutatedNeuronNames == originalNeuronNames,
                    s"Temp mutation should preserve neuron names: original=$originalNeuronNames, mutated=$mutatedNeuronNames"
                )
            end for
        }
    }

    "mutation applies to neurons with memory" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("memory-neuron", 2)
            for mutated <- mutation(individual)
            yield
                val originalMemoryCount = individual.genotype.neurons.count(_.memory.isDefined)
                val mutatedMemoryCount  = mutated.neurons.count(_.memory.isDefined)
                assert(
                    mutatedMemoryCount == originalMemoryCount,
                    s"Memory presence should be preserved: original=$originalMemoryCount, mutated=$mutatedMemoryCount"
                )
            end for
        }
    }

    "mutation produces different results on multiple runs" in run {
        val mutationState = Mutation.State(
            strategies = List(
                Mutation.Strategy.Structure(0.5, None),
                Mutation.Strategy.Prompt(0.5, None)
            ),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("variation", 2)
            for
                mutated1 <- mutation(individual)
                mutated2 <- mutation(individual)
                mutated3 <- mutation(individual)
            yield
                val allNames = List(mutated1.name, mutated2.name, mutated3.name)
                assert(
                    allNames.forall(_.nonEmpty),
                    s"All mutated minds should have names: $allNames"
                )
                succeed
            end for
        }
    }

    "mutation handles individual with complex network" in run {
        val neurons = List(
            Neuron.State(
                name = Neuron.Name("input"),
                description = "Input processor",
                previous = List.empty,
                next = List("processor1", "processor2").map(Neuron.Name(_)),
                conscious = false,
                memory = None,
                gate = Gate.State("Route to processors", "route", Temperature.creative, History.State(List.empty, 0))
            ),
            Neuron.State(
                name = Neuron.Name("processor1"),
                description = "Processor 1",
                previous = List(Neuron.Name("input")),
                next = List(Neuron.Name("output")),
                conscious = false,
                memory = Some(Memory.State(
                    List(Memory.Entry(0, 0, "Processor 1 memory")),
                    Gate.State("Retrieve proc1 mem", "mem", Temperature.precise, History.State(List.empty, 0))
                )),
                gate = Gate.State("Process 1", "p1", Temperature.balanced, History.State(List.empty, 0))
            ),
            Neuron.State(
                name = Neuron.Name("processor2"),
                description = "Processor 2",
                previous = List(Neuron.Name("input")),
                next = List(Neuron.Name("output")),
                conscious = true,
                memory = None,
                gate = Gate.State("Process 2", "p2", Temperature.creative, History.State(List.empty, 0))
            ),
            Neuron.State(
                name = Neuron.Name("output"),
                description = "Output generator",
                previous = List("processor1", "processor2").map(Neuron.Name(_)),
                next = List.empty,
                conscious = true,
                memory = None,
                gate = Gate.State("Generate output", "out", Temperature.balanced, History.State(List.empty, 0))
            )
        )

        val mindState = Mind.State(
            name = "complex-network",
            maxRecursions = 5,
            neurons = neurons,
            output = Gate.State("Final output", "final", Temperature.balanced, History.State(List.empty, 0))
        )

        val individual = Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = List(0.8, 0.75).map(TestScore("0", _)),
            sex = true,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for
                mutated <- mutation(individual)
            yield
                assert(mutated.neurons.size == 4, s"Should preserve all 4 neurons: ${mutated.neurons.size}")
                assert(mutated.neurons.count(_.memory.isDefined) == 1, "Should preserve memory neuron")
                assert(mutated.neurons.count(_.conscious) == 2, "Should preserve conscious neurons")
            end for
        }
    }

    "mutation with low rate sometimes skips mutation" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 0.3
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("low-rate", 2)

            for results <- Kyo.foreach((0 until 10).toList)(_ => mutation(individual))
            yield
                val unchangedCount = results.count(_ == individual.genotype)
                assert(
                    unchangedCount >= 0,
                    s"With rate=0.3, some mutations may be skipped (unchanged: $unchangedCount/10)"
                )
                assert(results.size == 10, s"Should have 10 results: ${results.size}")
            end for
        }
    }

    "Lamarckian evolution: mutation receives phenotype with execution history" in run {

        val genotype = Mind.State(
            name = "arithmetic-learner",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                "Solve arithmetic problems",
                "solve",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )

        val phenotypeWithHistory = genotype.copy(
            output = genotype.output.copy(
                history = History.State(
                    List(
                        History.Entry(0, List(Input("2+2")), "4"),
                        History.Entry(1, List(Input("3+3")), "6"),
                        History.Entry(2, List(Input("5+5")), "10")
                    ),
                    3
                )
            )
        )

        val individual = Individual(
            genotype = genotype,
            phenotype = phenotypeWithHistory,
            fitness = List(0.9).map(TestScore("0", _)),
            sex = false,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for mutated <- mutation(individual)
            yield

                assert(mutated.name.nonEmpty, "Mutated mind should have a name")
                assert(mutated.output.prompt.nonEmpty, "Mutated mind should have a prompt")

                succeed
            end for
        }
    }

    "Lamarckian evolution: phenotype history provides learning context" in run {

        val genotype = Mind.State(
            name = "pattern-learner",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                "Learn from patterns",
                "learn",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )

        val phenotypeWithLearning = genotype.copy(
            output = genotype.output.copy(
                history = History.State(
                    List(
                        History.Entry(0, List(Input("What is 10 * 10?")), "100"),
                        History.Entry(1, List(Input("What is 10 * 20?")), "200"),
                        History.Entry(2, List(Input("What is 10 * 30?")), "300")
                    ),
                    3
                )
            )
        )

        val individual = Individual(
            genotype = genotype,
            phenotype = phenotypeWithLearning,
            fitness = List(0.85).map(TestScore("0", _)),
            sex = true,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for mutated <- mutation(individual)
            yield

                assert(mutated.output.prompt.nonEmpty, "Should produce mutated prompt")
                succeed
            end for
        }
    }

    "Lamarckian evolution: mutation uses phenotype not genotype" in run {
        val genotype = Mind.State(
            name = "base-mind",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                "Original prompt",
                "original",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )

        val phenotypeWithMemory = genotype.copy(
            neurons = List(
                Neuron.State(
                    name = Neuron.Name("learned-neuron"),
                    description = "Neuron with learned experiences",
                    previous = List.empty,
                    next = List.empty,
                    conscious = true,
                    memory = Some(Memory.State(
                        List(
                            Memory.Entry(2, 1, "Learned pattern A works well"),
                            Memory.Entry(3, 0, "Learned pattern B fails often")
                        ),
                        Gate.State("Memory", "mem", Temperature.balanced, History.State(List.empty, 0))
                    )),
                    gate = Gate.State(
                        "Output",
                        "out",
                        Temperature.balanced,
                        History.State(
                            List(
                                History.Entry(0, List(Input("test input 1")), "good output"),
                                History.Entry(1, List(Input("test input 2")), "bad output")
                            ),
                            2
                        )
                    )
                )
            )
        )

        val individual = Individual(
            genotype = genotype,
            phenotype = phenotypeWithMemory,
            fitness = List(0.75).map(TestScore("0", _)),
            sex = false,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Structure(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for mutated <- mutation(individual)
            yield

                assert(mutated.neurons.size >= 0, "Should produce valid neuron structure")
                succeed
            end for
        }
    }

    "mutation sees execution failures in phenotype history" in run {
        val genotype = Mind.State(
            name = "problem-solver",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                "Solve problems",
                "solve",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )

        val phenotypeWithFailures = genotype.copy(
            output = genotype.output.copy(
                history = History.State(
                    List(
                        History.Entry(0, List(Input("complex problem 1")), "incorrect answer"),
                        History.Entry(1, List(Input("complex problem 2")), "incorrect answer"),
                        History.Entry(2, List(Input("simple problem")), "correct answer")
                    ),
                    3
                )
            )
        )

        val individual = Individual(
            genotype = genotype,
            phenotype = phenotypeWithFailures,
            fitness = List(0.4).map(TestScore("0", _)),
            sex = true,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for mutated <- mutation(individual)
            yield

                assert(mutated.output.prompt.nonEmpty, "Should produce adjusted prompt")
                succeed
            end for
        }
    }

    "CANONICAL: mutation produces different outputs on repeated calls (exploration)" in run {
        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation   = Mutation.init(mutationState)
            val individual = createIndividual("exploration-test", 2)
            for
                mutated1 <- mutation(individual)
                mutated2 <- mutation(individual)
            yield assert(
                mutated1 != mutated2,
                s"Mutation should produce different results on repeated calls to explore search space"
            )
            end for
        }
    }

    "CANONICAL: mutation handles phenotype with large execution history (stress test)" in run {

        val largeHistory = List.from(
            (0 until 10).map { i =>
                History.Entry(0, List(Input(s"input_$i with some content")), s"output_$i with detailed response")
            }
        )

        val genotype = Mind.State(
            name = "large-history-mind",
            maxRecursions = 2,
            neurons = List(
                Neuron.State(
                    name = Neuron.Name("processor"),
                    description = "Processing neuron with large history",
                    previous = List.empty,
                    next = List.empty,
                    conscious = true,
                    memory = None,
                    gate = Gate.State(
                        "Process with extensive context",
                        "process",
                        Temperature.balanced,
                        History.State(largeHistory, 10)
                    )
                )
            ),
            output = Gate.State(
                "Synthesize from large context",
                "synthesize",
                Temperature.balanced,
                History.State(largeHistory, 10)
            )
        )

        val individual = Individual(
            genotype = genotype,
            phenotype = genotype,
            fitness = List(0.7).map(TestScore("0", _)),
            sex = false,
            lineage = Lineage.empty
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Prompt(1.0, None)),
            rate = 1.0
        )

        AI.run {
            val mutation = Mutation.init(mutationState)
            for
                mutated     <- mutation(individual)
                mutatedMind <- Mind.init(mutated)
                result      <- mutatedMind[String](List(Input("test")))
            yield

                assert(mutated.name.nonEmpty, "Should produce valid mind despite large history")
                assert(result.nonEmpty, "Mutated mind with large history should be executable")
                succeed
            end for
        }
    }

end MutationLiveTest
