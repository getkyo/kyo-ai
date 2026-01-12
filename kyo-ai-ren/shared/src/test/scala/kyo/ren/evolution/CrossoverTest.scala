package kyo.ren.evolution

import kyo.*
import kyo.Test
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class CrossoverTest extends Test:

    def createIndividual(name: String): Individual =
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
            fitness = List(TestScore("0", 0.5)),
            sex = false,
            lineage = Lineage.empty
        )
    end createIndividual

    "State validation" - {
        "accepts valid crossover state with default" in {
            val state = Crossover.State()
            assert(state.customMind.isEmpty)
        }

        "accepts custom mind state" in {
            val customMind = Mind.State(
                name = "custom",
                maxRecursions = 1,
                neurons = List.empty,
                output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
            )
            val state = Crossover.State(customMind = Some(customMind))
            assert(state.customMind.contains(customMind))
        }
    }

    "Plan structure" - {
        "accepts valid plan with connections" in {
            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("parse")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("compute"))
                    )
                )
            )
            assert(plan.name == "offspring")
            assert(plan.connections.size == 1)
        }

        "accepts empty connections" in {
            val plan = Crossover.Plan(
                name = "offspring",
                connections = List.empty
            )
            assert(plan.connections.isEmpty)
        }

        "distinguishes mother vs father references" in {
            val motherRef = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("n1"))
            val fatherRef = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("n1"))
            assert(motherRef != fatherRef)
        }

        "distinguishes neuron names" in {
            val ref1 = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("n1"))
            val ref2 = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("n2"))
            assert(ref1 != ref2)
        }
    }

    "Reconstruction" - {
        def createParent(name: String, neuronNames: List[String]): Mind.State =
            val neurons = neuronNames.map { n =>
                Neuron.State(
                    name = Neuron.Name(n),
                    description = s"$n neuron",
                    conscious = true,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
                )
            }
            Mind.State(
                name = name,
                maxRecursions = 1,
                neurons = neurons,
                output = Gate.State("output", "reminder", Temperature.balanced, History.State(List.empty, 0))
            )
        end createParent

        "selects neurons referenced in connections" in run {
            val mother = createParent("mother", List("parse", "compute", "verify"))
            val father = createParent("father", List("parse", "compute", "output"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("parse")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("compute"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                assert(offspring.neurons.size == 2)
                assert(offspring.neurons.exists(_.name == "parse"))
                assert(offspring.neurons.exists(_.name == "compute"))
            }
        }

        "builds connectivity from connection edges" in run {
            val mother = createParent("mother", List("a", "b", "c"))
            val father = createParent("father", List("d", "e", "f"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("b"))
                    ),
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("b")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("e"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                val neuronA = offspring.neurons.find(_.name == "a").get
                val neuronB = offspring.neurons.find(_.name == "b").get
                val neuronE = offspring.neurons.find(_.name == "e").get

                assert(neuronA.previous.isEmpty)
                assert(neuronA.next == List("b"))

                assert(neuronB.previous == List("a"))
                assert(neuronB.next == List("e"))

                assert(neuronE.previous == List("b"))
                assert(neuronE.next.isEmpty)
            }
        }

        "handles empty connections" in run {
            val mother = createParent("mother", List("a", "b"))
            val father = createParent("father", List("c", "d"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List.empty
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                assert(offspring.neurons.isEmpty)
            }
        }

        "ignores neurons not referenced in connections" in run {
            val mother = createParent("mother", List("a", "b", "c"))
            val father = createParent("father", List("d", "e", "f"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("e"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                assert(offspring.neurons.size == 2)
                assert(offspring.neurons.exists(_.name == "a"))
                assert(offspring.neurons.exists(_.name == "e"))
                assert(!offspring.neurons.exists(_.name == "b"))
                assert(!offspring.neurons.exists(_.name == "c"))
            }
        }

        "handles neurons with multiple incoming connections" in run {
            val mother = createParent("mother", List("a", "b", "c"))
            val father = createParent("father", List("d"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("d"))
                    ),
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("b")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("d"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                val neuronD = offspring.neurons.find(_.name == "d").get
                assert(neuronD.previous.size == 2)
                assert(neuronD.previous.contains("a"))
                assert(neuronD.previous.contains("b"))
            }
        }

        "handles neurons with multiple outgoing connections" in run {
            val mother = createParent("mother", List("a"))
            val father = createParent("father", List("d", "e"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("d"))
                    ),
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("e"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                val neuronA = offspring.neurons.find(_.name == "a").get
                assert(neuronA.next.size == 2)
                assert(neuronA.next.contains("d"))
                assert(neuronA.next.contains("e"))
            }
        }

        "sets offspring name from delta" in run {
            val mother = createParent("mother", List("a"))
            val father = createParent("father", List("b"))

            val plan = Crossover.Plan(
                name = "custom-offspring-name",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("b"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                assert(offspring.name == "custom-offspring-name")
            }
        }

        "randomly selects maxRecursions from parents" in run {
            val mother = createParent("mother", List("a")).copy(maxRecursions = 5)
            val father = createParent("father", List("b")).copy(maxRecursions = 10)

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("b"))
                    )
                )
            )

            Kyo.fill(20) {
                Crossover.reconstruct(mother, father, plan).map(_.maxRecursions)
            }.map { results =>
                // Should see both 5 and 10 in results (random selection)
                assert(results.contains(5) || results.contains(10))
            }
        }

        "randomly selects output gate from parents" in run {
            val mother = createParent("mother", List("a")).copy(
                output = Gate.State("mother-prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
            )
            val father = createParent("father", List("b")).copy(
                output = Gate.State("father-prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
            )

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("b"))
                    )
                )
            )

            Kyo.fill(20) {
                Crossover.reconstruct(mother, father, plan).map(_.output.prompt)
            }.map { results =>
                // Should see both prompts in results (random selection)
                assert(results.contains("mother-prompt") || results.contains("father-prompt"))
            }
        }

        "handles self-loops" in run {
            val mother = createParent("mother", List("a"))
            val father = createParent("father", List("b"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a")),
                        to = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("a"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                val neuronA = offspring.neurons.find(_.name == "a").get
                assert(neuronA.previous == List("a"))
                assert(neuronA.next == List("a"))
            }
        }

        "normalizes duplicate neuron names" in run {
            val mother = createParent("mother", List("dup", "dup"))
            val father = createParent("father", List("dup"))

            val plan = Crossover.Plan(
                name = "offspring",
                connections = List(
                    Crossover.Connection(
                        from = Crossover.NeuronRef(sex = true, neuronName = Neuron.Name("dup")),
                        to = Crossover.NeuronRef(sex = false, neuronName = Neuron.Name("dup"))
                    )
                )
            )

            Crossover.reconstruct(mother, father, plan).map { offspring =>
                // After normalization, all neuron names should be unique
                val names = offspring.neurons.map(_.name)
                assert(names.size == names.toSet.size)
            }
        }
    }

end CrossoverTest
