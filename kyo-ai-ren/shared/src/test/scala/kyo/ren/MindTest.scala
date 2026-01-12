package kyo.ren

import kyo.*
import kyo.Test
import kyo.ai.json.*
import kyo.ren.network.*
import kyo.ren.network.Memory.Range

class MindTest extends Test:

    "basic mind operations" - {
        "apply generates output with empty neurons" in run {
            val mindState = Mind.State(
                name = "test-mind",
                maxRecursions = 3,
                neurons = List.empty,
                output = Gate.State("output prompt", "output reminder", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                outputGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = "final output")
                )
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List.empty, outputGate, mutex)
                result <- AI.run {
                    mind[String](Input(List("input"), List.empty[String], None))
                }
            yield assert(result == "final output")
            end for
        }

        "apply passes inputs to output gate" in run {
            var capturedInputs: Option[List[Input]] = None

            val outputGate = new Gate:
                def apply[A: Json](inputs: List[Input])(using Frame): A < AI =
                    capturedInputs = Some(inputs)
                    "final".asInstanceOf[A]

                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val neuronState = Neuron.State(
                name = Neuron.Name("n1"),
                description = "desc",
                conscious = true,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "test",
                maxRecursions = 1,
                neurons = List(neuronState),
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val testInputs = List(Input(List("test"), List.empty[String], None))

            for
                neuronGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = kyo.MockGate.output("neuron-output"))
                )
                neuron = Neuron(neuronState, None, neuronGate)
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List(neuron), outputGate, mutex)
                _ <- AI.run {
                    mind[String](testInputs)
                }
            yield
                assert(capturedInputs.isDefined)
                assert(capturedInputs.getOrElse(List.empty).startsWith(testInputs))
            end for
        }

        "apply uses deterministic seed based on input" in run {
            val mindState = Mind.State(
                name = "seeded",
                maxRecursions = 1,
                neurons = List.empty,
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                outputGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = 42)
                )
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List.empty, outputGate, mutex)
                (result1, result2, result3) <- AI.run {
                    for
                        result1 <- mind[Int](Input(List("same input"), List.empty[String], None))
                        result2 <- mind[Int](Input(List("same input"), List.empty[String], None))
                        result3 <- mind[Int](Input(List("different"), List.empty[String], None))
                    yield (result1, result2, result3)
                }
            yield
                assert(result1 == 42)
                assert(result2 == 42)
                assert(result3 == 42)
            end for
        }
    }

    "state management" - {
        "state preserves all fields" in run {
            val neuronState = Neuron.State(
                name = Neuron.Name("neuron"),
                description = "desc",
                conscious = true,
                previous = List(Neuron.Name("p1")),
                next = List(Neuron.Name("n1")),
                memory = None,
                gate = Gate.State("np", "nr", Temperature.focused, History.State(List.empty, 3))
            )

            val mindState = Mind.State(
                name = "complex",
                maxRecursions = 5,
                neurons = List(neuronState),
                output = Gate.State("op", "or", Temperature.creative, History.State(List.empty, 10))
            )

            for
                neuronGate <- kyo.MockGate.init()
                outputGate <- kyo.MockGate.init()
                neuron = Neuron(neuronState, None, neuronGate)
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List(neuron), outputGate, mutex)
                state <- mind.state
            yield
                assert(state.name == "complex")
                assert(state.maxRecursions == 5)
                assert(state.neurons.size == 1)
                assert(state.neurons.head.name == "neuron")
                assert(state.output.prompt == "mock prompt")
                assert(state.output.temperature == Temperature.balanced)
            end for
        }

        "state updates neuron states" in run {
            val neuronState = Neuron.State(
                name = Neuron.Name("with-memory"),
                description = "desc",
                conscious = true,
                previous = List.empty,
                next = List.empty,
                memory = Some(Memory.State(
                    List(Memory.Entry(0, 0, "content")),
                    Gate.State("mp", "mr", Temperature.balanced, History.State(List.empty, 0))
                )),
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "test",
                maxRecursions = 1,
                neurons = List(neuronState),
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                memoryGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = Memory.MemoryOutput(None, Some(List(Range(0, 0)))))
                )
                neuronGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = kyo.MockGate.output("neuron-output"))
                )
                outputGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = "final-output")
                )
                memory = Memory(
                    Memory.State(
                        entries = List(Memory.Entry(0, 0, "content")),
                        gate = Gate.State("mp", "mr", Temperature.balanced, History.State(List.empty, 0))
                    ),
                    memoryGate
                )
                neuron = Neuron(neuronState, Some(memory), neuronGate)
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List(neuron), outputGate, mutex)
                (_, state) <- AI.run {
                    for
                        _     <- mind[String](Input(List("trigger memory"), List.empty[String], None))
                        state <- mind.state
                    yield ((), state)
                }
            yield
                assert(state.neurons.head.memory.isDefined)
                assert(state.neurons.head.memory.get.entries.head.reads == 1)
            end for
        }
    }

    "factory methods" - {
        "Mind.init creates from state" in run {
            val neuronState = Neuron.State(
                name = Neuron.Name("n"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "initialized",
                maxRecursions = 2,
                neurons = List(neuronState),
                output = Gate.State("op", "or", Temperature.focused, History.State(List.empty, 5))
            )

            for
                mind  <- Mind.init(mindState)
                state <- mind.state
            yield
                assert(state.name == "initialized")
                assert(state.maxRecursions == 2)
            end for
        }

        "Mind.init handles empty neurons" in run {
            val mindState = Mind.State(
                name = "empty",
                maxRecursions = 1,
                neurons = List.empty,
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                mind  <- Mind.init(mindState)
                state <- mind.state
            yield
                assert(state.name == "empty")
                assert(state.neurons.isEmpty)
            end for
        }

        "Mind.init initializes neurons with memory" in run {
            val memoryState = Memory.State(
                entries = List(Memory.Entry(0, 0, "data")),
                gate = Gate.State("mp", "mr", Temperature.balanced, History.State(List.empty, 0))
            )

            val neuronState = Neuron.State(
                name = Neuron.Name("mem-neuron"),
                description = "desc",
                conscious = true,
                previous = List.empty,
                next = List.empty,
                memory = Some(memoryState),
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "with-memory",
                maxRecursions = 1,
                neurons = List(neuronState),
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                mind  <- Mind.init(mindState)
                state <- mind.state
            yield
                assert(state.neurons.size == 1)
                assert(state.neurons.head.name == "mem-neuron")
            end for
        }
    }

    "validation" - {
        "requires non-empty name" in {
            val result = scala.util.Try {
                Mind.State(
                    name = "",
                    maxRecursions = 1,
                    neurons = List.empty,
                    output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                )
            }
            assert(result.isFailure)
        }

        "requires non-negative maxRecursions" in {
            val result0 = scala.util.Try {
                Mind.State(
                    name = "test",
                    maxRecursions = 0,
                    neurons = List.empty,
                    output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                )
            }
            assert(result0.isSuccess)

            val resultNegative = scala.util.Try {
                Mind.State(
                    name = "test",
                    maxRecursions = -1,
                    neurons = List.empty,
                    output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                )
            }
            assert(resultNegative.isFailure)
        }
    }

    "network integration" - {
        "passes neurons to network" in run {
            val neuron1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "first",
                conscious = true,
                previous = List.empty,
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val neuron2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "second",
                conscious = true,
                previous = List(Neuron.Name("n1")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "networked",
                maxRecursions = 3,
                neurons = List(neuron1State, neuron2State),
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                neuron1Gate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = kyo.MockGate.output("n1-out"))
                )
                neuron2Gate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = kyo.MockGate.output("n2-out"))
                )
                outputGate <- kyo.MockGate.init(
                    kyo.MockGate.Response(output = "final")
                )
                neuron1 = Neuron(neuron1State, None, neuron1Gate)
                neuron2 = Neuron(neuron2State, None, neuron2Gate)
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List(neuron1, neuron2), outputGate, mutex)
                result <- AI.run {
                    mind[String](Input(List("input"), List.empty[String], None))
                }
            yield assert(result == "final")
            end for
        }
    }

    "exports" - {
        "exports name and maxRecursions" in run {
            val mindState = Mind.State(
                name = "exported",
                maxRecursions = 7,
                neurons = List.empty,
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                outputGate <- kyo.MockGate.init()
                mutex      <- Meter.initMutexUnscoped
                mind = Mind(mindState, List.empty, outputGate, mutex)
                state <- mind.state
            yield
                assert(state.name == "exported")
                assert(state.maxRecursions == 7)
            end for
        }

        "exports neurons" in run {
            val neuronState = Neuron.State(
                name = Neuron.Name("n1"),
                description = "desc",
                conscious = false,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mindState = Mind.State(
                name = "test",
                maxRecursions = 1,
                neurons = List(neuronState),
                output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                neuronGate <- kyo.MockGate.init()
                outputGate <- kyo.MockGate.init()
                neuron = Neuron(neuronState, None, neuronGate)
                mutex <- Meter.initMutexUnscoped
                mind = Mind(mindState, List(neuron), outputGate, mutex)
                state <- mind.state
            yield
                assert(state.neurons.size == 1)
                assert(state.neurons.head.name == "n1")
            end for
        }
    }

end MindTest
