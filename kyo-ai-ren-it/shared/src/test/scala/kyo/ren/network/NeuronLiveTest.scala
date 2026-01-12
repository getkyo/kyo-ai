package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ren.*

class NeuronLiveTest extends Test:

    "basic neuron with output only" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("analyzer"),
            description = "Analyzes input and produces output",
            previous = List.empty,
            next = List(Neuron.Name("processor")),
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Generate a brief analysis of the input text.
                    Return a string containing your analysis.""",
                p"""Return a concise string analysis.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron <- network.Neuron.init(initialState)
                reachable = List.empty[Neuron]
                output <- neuron(List(Input("The sky is blue")), reachable)
            yield assert(output.output.nonEmpty, "Output should not be empty")
            end for
        }
    }

    "neuron with memory integration" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("memory-neuron"),
            description = "Processes input with memory",
            previous = List.empty,
            next = List.empty,
            conscious = true,
            memory = Some(network.Memory.State(
                List(
                    network.Memory.Entry(0, 0, "The chemical formula for water is H2O")
                ),
                network.Gate.State(
                    p"""Generate MemoryOutput to retrieve relevant entries for the input.
                        Use the retrieve field to fetch entries by index ranges.""",
                    p"""Return MemoryOutput with appropriate retrieve ranges.""",
                    Temperature.balanced,
                    network.History.State(List.empty, 0)
                )
            )),
            gate = network.Gate.State(
                p"""Generate output based on the input and memory context.
                    Your response must include the exact chemical formula from memory.""",
                p"""Generate informed response with exact formula.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron <- network.Neuron.init(initialState)
                output <- neuron(List(Input("What is the chemical formula for water?")), List.empty)
            yield
                assert(output.output.getOrElse("").nonEmpty, "Output should not be empty")
                assert(output.output.get.contains("H2O"), s"Output must contain exact formula 'H2O': $output")
            end for
        }
    }

    "neuron without routing creates activations for all reachable" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("broadcaster"),
            description = "Broadcasts to all reachable neurons",
            previous = List.empty,
            next = List("neuron1", "neuron2").map(Neuron.Name(_)),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Process the input and return a string output.""",
                p"""Return string output.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron  <- network.Neuron.init(initialState)
                neuron1 <- createSimpleNeuron("neuron1", "First target")
                neuron2 <- createSimpleNeuron("neuron2", "Second target")
                reachable = List(neuron1, neuron2)
                output <- neuron(List(Input("broadcast this")), reachable)
            yield assert(
                output.activations.getOrElse(List.empty).flatMap(_.neuronNames).toSet == Set("neuron1", "neuron2"),
                s"Should target both neurons: ${output.activations.getOrElse(List.empty).map(_.neuronNames)}"
            )
            end for
        }
    }

    "neuron with routing selects specific neurons" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("router"),
            description = "Routes to specific neurons based on input",
            previous = List.empty,
            next = List("math", "science", "history").map(Neuron.Name(_)),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Based on the input and available neurons, generate activations for the most relevant neurons.
                    The context contains available neurons with their descriptions.
                    Return a List of Activation objects targeting appropriate neurons.""",
                p"""Route to relevant neurons based on input.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron  <- network.Neuron.init(initialState)
                math    <- createSimpleNeuron("math", "Handles mathematical questions")
                science <- createSimpleNeuron("science", "Handles science questions")
                history <- createSimpleNeuron("history", "Handles history questions")
                reachable = List(math, science, history)
                output <- neuron(List(Input("What is 2 + 2?")), reachable)
            yield
                assert(output.activations.nonEmpty, "Should activate at least one neuron")
                assert(
                    output.activations.getOrElse(List.empty).exists(_.neuronNames == List("math")),
                    s"Should route to math neuron for math question, but got: ${output.activations.getOrElse(List.empty).map(_.neuronNames)}"
                )
            end for
        }
    }

    "neuron with memory and routing combined" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("complex"),
            description = "Complex neuron with both memory and routing",
            previous = List.empty,
            next = List("action1", "action2").map(Neuron.Name(_)),
            conscious = true,
            memory = Some(network.Memory.State(
                List(
                    network.Memory.Entry(0, 0, "user prefers action1")
                ),
                network.Gate.State(
                    p"""Generate MemoryOutput to manage memory based on input.""",
                    p"""Return valid MemoryOutput.""",
                    Temperature.precise,
                    network.History.State(List.empty, 0)
                )
            )),
            gate = network.Gate.State(
                p"""Route to appropriate neurons based on input and memory context.
                    Context provides neurons in format 'name: description'.
                    In the Activation objects, the 'to' field must contain only the neuron name (before the colon).
                    IMPORTANT: Since memory indicates 'user prefers action1', you should route to action1.""",
                p"""Route using neuron names only.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron  <- network.Neuron.init(initialState)
                action1 <- createSimpleNeuron("action1", "First action")
                action2 <- createSimpleNeuron("action2", "Second action")
                reachable = List(action1, action2)
                output <- neuron(List(Input("perform preferred action")), reachable)
            yield
                assert(output.activations.nonEmpty, "Should activate at least one neuron")
                val activated = output.activations.getOrElse(List.empty).flatMap(_.neuronNames).toSet
                assert(
                    activated.contains(Neuron.Name("action1")),
                    s"Should route to 'action1' based on memory preference, but got: ${output.activations.getOrElse(List.empty).map(_.neuronNames)}"
                )
            end for
        }
    }

    "activation input is prefixed with neuron name" in run {
        val initialState = network.Neuron.State(
            name = Neuron.Name("source-neuron"),
            description = "Source neuron",
            previous = List.empty,
            next = List(Neuron.Name("target")),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Return the string 'hello'.""",
                p"""Return 'hello'.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                neuron <- network.Neuron.init(initialState)
                target <- createSimpleNeuron("target", "Target neuron")
                reachable = List(target)
                output <- neuron(List(Input("test")), reachable)
            yield
                assert(output.activations.size == 1, "Should have one activation")
                assert(
                    output.activations.getOrElse(List.empty).head.tasks.get.head.contains("hello"),
                    s"Activation input should be prefixed with neuron name: ${output.activations.getOrElse(List.empty).head.tasks.head}"
                )
            end for
        }
    }

    def createSimpleNeuron(name: String, description: String)(using Frame): Neuron < Sync =
        val state = network.Neuron.State(
            name = Neuron.Name(name),
            description = description,
            previous = List.empty,
            next = List.empty,
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Process the input and return a simple acknowledgment.""",
                p"""Return simple output.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )
        network.Neuron.init(state)
    end createSimpleNeuron

end NeuronLiveTest
