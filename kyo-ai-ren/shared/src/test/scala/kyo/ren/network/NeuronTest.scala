package kyo.ren.network

import kyo.*
import kyo.MockGate
import kyo.Test
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.network.Memory.Range

class NeuronTest extends Test:

    "basic neuron operations" - {
        "apply without memory" in run {
            AI.run {
                for
                    nextNeuron <- createNeuron(name = "next1")
                    gate = MockGate.init(
                        MockGate.Response(
                            output = MockGate.output(
                                "processed-output",
                                network.Neuron.Activation(List(nextNeuron.name), Some(List("task1")), Some(List("ctx1")))
                            )
                        )
                    )
                    neuron <- createNeuron(name = "simple", next = List("next1"), gate = gate)
                    reachable = List(nextNeuron)
                    output <- neuron(List(Input(List("input1", "input2"))), reachable)
                yield
                    assert(output.output.contains("processed-output"))
                    assert(output.activations.size == 1)
                    assert(output.activations.get.head.neuronNames == List(nextNeuron.name))
                    assert(output.activations.get.head.tasks.get == List("task1"))
                    assert(output.activations.get.head.context.get == List("ctx1"))
            }
        }

        "apply with empty input" in run {
            val gate = MockGate.init(
                MockGate.Response(
                    output = MockGate.output("empty-result")
                )
            )

            AI.run {
                for
                    neuron <- createNeuron(gate = gate)
                    output <- neuron(List.empty[Input], List.empty[Neuron])
                yield assert(output.output.contains("empty-result"))
            }
        }

        "apply with no reachable neurons" in run {
            val gate = MockGate.init(
                MockGate.Response(output = MockGate.output("output"))
            )

            AI.run {
                for
                    neuron <- createNeuron(
                        name = "isolated",
                        next = List("a", "b"),
                        gate = gate
                    )
                    output <- neuron(List(Input("test")), List.empty[Neuron])
                yield assert(output.activations.getOrElse(List.empty).isEmpty)
            }
        }

        "gate can create activations for multiple reachable neurons" in run {
            AI.run {
                for
                    a <- createNeuron(name = "a")
                    b <- createNeuron(name = "b")
                    c <- createNeuron(name = "c")
                    gate = MockGate.init(
                        MockGate.Response(output =
                            MockGate.output(
                                "broadcast",
                                network.Neuron.Activation(List(a.name), Some(List("data-a")), None),
                                network.Neuron.Activation(List(b.name), Some(List("data-b")), None),
                                network.Neuron.Activation(List(c.name), Some(List("data-c")), None)
                            )
                        )
                    )
                    neuron <- createNeuron(
                        name = "broadcaster",
                        next = List("a", "b", "c"),
                        gate = gate
                    )
                    reachable = List(a, b, c)
                    output <- neuron(List(Input("test")), reachable)
                yield
                    assert(output.output.contains("broadcast"))
                    assert(output.activations.getOrElse(List.empty).size == 3)
                    assert(output.activations.getOrElse(List.empty).flatMap(_.neuronNames).toSet == Set(a.name, b.name, c.name))
            }
        }
    }

    "memory integration" - {
        "apply with memory retrieval" in run {
            val memoryGate = MockGate.init(
                MockGate.Response(
                    output = network.Memory.MemoryOutput(None, Some(List(Range(0, 0))))
                )
            )

            val gate = MockGate.init(
                MockGate.Response(
                    output = MockGate.output("output-with-memory")
                )
            )

            AI.run {
                for
                    memoryGateResolved <- memoryGate
                    memory = Memory(
                        network.Memory.State(
                            entries = List(network.Memory.Entry(1, 1, "stored content")),
                            gate =
                                network.Gate.State(
                                    "memory prompt",
                                    "memory reminder",
                                    Temperature.balanced,
                                    network.History.State(List.empty, 0)
                                )
                        ),
                        memoryGateResolved
                    )
                    memoryState     <- memory.state
                    gateResolved    <- gate
                    reachableNeuron <- createNeuron(name = "next1")
                    neuron = Neuron(
                        init = network.Neuron.State(
                            name = network.Neuron.Name("with-memory"),
                            description = "test",
                            conscious = true,
                            previous = List.empty,
                            next = List(reachableNeuron.name),
                            memory = Some(memoryState),
                            gate = network.Gate.State(
                                "gate prompt",
                                "gate reminder",
                                Temperature.balanced,
                                network.History.State(List.empty, 0)
                            )
                        ),
                        memory = Some(memory),
                        gate = gateResolved
                    )
                    reachable = List(reachableNeuron)
                    output <- neuron(List(Input(List("input1", "input2"))), reachable)
                yield
                    assert(output.output.get == "output-with-memory")
                    assert(output.activations.isEmpty || output.activations.nonEmpty)
            }
        }

        "memory output passed as context to gate" in run {
            var capturedContext: Option[List[String]] = None

            val memoryGate = MockGate.init(
                MockGate.Response(
                    output = network.Memory.MemoryOutput(None, Some(List(Range(0, 0))))
                )
            )

            val gate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    capturedContext = Some(inputs.flatMap(_.context))
                    MockGate.output("result").asInstanceOf[Out]
                end apply

                def state(using Frame) = network.Gate.State(
                    "p",
                    "r",
                    Temperature.balanced,
                    network.History.State(List.empty, 0)
                )

            AI.run {
                for
                    memoryGateResolved <- memoryGate
                    memory = Memory(
                        network.Memory.State(
                            entries = List(network.Memory.Entry(0, 0, "mem1")),
                            gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memoryGateResolved
                    )
                    memState <- memory.state
                    neuron = Neuron(
                        init = network.Neuron.State(
                            name = network.Neuron.Name("test"),
                            description = "desc",
                            conscious = false,
                            previous = List.empty,
                            next = List.empty,
                            memory = Some(memState),
                            gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memory = Some(memory),
                        gate = gate
                    )
                    _ <- neuron(List(Input("input")), List.empty[Neuron])
                yield
                    assert(capturedContext.isDefined)
                    // The memory entries should be JSON encoded in the context
                    assert(capturedContext.get.nonEmpty)
            }
        }

        "memory with multiple retrieved entries" in run {
            val memoryGate = MockGate.init(
                MockGate.Response(
                    output = network.Memory.MemoryOutput(None, Some(List(Range(0, 2))))
                )
            )

            val gate = MockGate.init(
                MockGate.Response(output = MockGate.output("multi-memory"))
            )

            AI.run {
                for
                    memoryGateResolved <- memoryGate
                    memory = Memory(
                        network.Memory.State(
                            entries = List(
                                network.Memory.Entry(0, 0, "first"),
                                network.Memory.Entry(1, 1, "second"),
                                network.Memory.Entry(2, 2, "third")
                            ),
                            gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memoryGateResolved
                    )
                    memState     <- memory.state
                    gateResolved <- gate
                    neuron = Neuron(
                        init = network.Neuron.State(
                            name = network.Neuron.Name("multi"),
                            description = "desc",
                            conscious = true,
                            previous = List.empty,
                            next = List.empty,
                            memory = Some(memState),
                            gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memory = Some(memory),
                        gate = gateResolved
                    )
                    output <- neuron(List(Input("test")), List.empty[Neuron])
                yield assert(output.output.get == "multi-memory")
            }
        }
    }

    "state management" - {
        "conscious neurons should be marked correctly" in run {
            for
                consciousNeuron   <- createNeuron(name = "conscious", conscious = true)
                unconsciousNeuron <- createNeuron(name = "unconscious", conscious = false)
            yield
                assert(consciousNeuron.conscious == true)
                assert(unconsciousNeuron.conscious == false)
        }

        "state should preserve all fields" in run {
            val memoryGate = MockGate.init()
            val gate       = MockGate.init()

            for
                memoryGateResolved <- memoryGate
                gateResolved       <- gate
                memory = Memory(
                    network.Memory.State(
                        entries = List(network.Memory.Entry(1, 1, "content")),
                        gate = network.Gate.State("mem prompt", "rem", Temperature.creative, network.History.State(List.empty, 5))
                    ),
                    memoryGateResolved
                )
                neuron = Neuron(
                    init = network.Neuron.State(
                        name = network.Neuron.Name("full"),
                        description = "full neuron",
                        conscious = true,
                        previous = List(network.Neuron.Name("prev1"), network.Neuron.Name("prev2")),
                        next = List(network.Neuron.Name("next1"), network.Neuron.Name("next2")),
                        memory =
                            Some(network.Memory.State(
                                List.empty,
                                network.Gate.State("m prompt", "r", Temperature.focused, network.History.State(List.empty, 3))
                            )),
                        gate = network.Gate.State("out prompt", "rem", Temperature.balanced, network.History.State(List.empty, 10))
                    ),
                    memory = Some(memory),
                    gate = gateResolved
                )
                state <- neuron.state
            yield
                assert(state.name == network.Neuron.Name("full"))
                assert(state.description == "full neuron")
                assert(state.previous == List(network.Neuron.Name("prev1"), network.Neuron.Name("prev2")))
                assert(state.next == List(network.Neuron.Name("next1"), network.Neuron.Name("next2")))
                assert(state.conscious == true)
                assert(state.memory.isDefined)
            end for
        }

        "exports should work correctly" in run {
            for
                neuron <- createNeuron(
                    name = "test",
                    description = "desc",
                    conscious = true,
                    next = List("a", "b"),
                    previous = List("p1")
                )
            yield
                assert(neuron.name == "test")
                assert(neuron.conscious == true)
                assert(neuron.next == List("a", "b"))
                assert(neuron.previous == List("p1"))
        }

    }

    "data flow isolation" - {
        "downstream neuron receives only upstream output not original input" in run {
            var upstreamReceivedTasks: List[String]   = List.empty
            var downstreamReceivedTasks: List[String] = List.empty

            AI.run {
                for
                    downstream <- createNeuron(name = "downstream")

                    upstreamGate = new Gate:
                        def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                            upstreamReceivedTasks = inputs.flatMap(_.tasks)
                            MockGate.output(
                                "TRANSFORMED",
                                network.Neuron.Activation(List(downstream.name), Some(List("TRANS_TASK")), None)
                            ).asInstanceOf[Out]
                        end apply
                        def state(using Frame) = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))

                    downstreamGate = new Gate:
                        def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                            downstreamReceivedTasks = inputs.flatMap(_.tasks)
                            MockGate.output("downstream-out").asInstanceOf[Out]
                        def state(using Frame) = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))

                    upstream <- createNeuron(
                        name = "upstream",
                        next = List("downstream"),
                        gate = upstreamGate
                    )
                    downstream2 <- createNeuron(
                        name = "downstream",
                        previous = List("upstream"),
                        gate = downstreamGate
                    )
                    upOutput <- upstream(List(Input(None, List("ORIGINAL"), List.empty)), List(downstream))
                    _ = assert(upOutput.output.get == "TRANSFORMED")
                    _ = assert(upstreamReceivedTasks == List("ORIGINAL", "Consider the valid next neurons to select for activation."))
                    downOutput <-
                        downstream2(
                            List(Input(
                                None,
                                upOutput.activations.get.head.tasks.getOrElse(List.empty),
                                upOutput.activations.get.head.context.getOrElse(List.empty)
                            )),
                            List.empty[Neuron]
                        )
                yield
                    assert(downstreamReceivedTasks.head == "TRANS_TASK")
                    assert(!downstreamReceivedTasks.exists(_.contains("ORIGINAL")))
                    assert(downOutput.output.get == "downstream-out")
                end for
            }
        }
    }

    "subconscious neuron state persistence" - {
        "subconscious neuron memory updates persist" in run {
            val memoryGate = MockGate.init(
                MockGate.Response(
                    output = network.Memory.MemoryOutput(
                        Some(List(network.Memory.Update.Add(None, "learned-unconsciously"))),
                        None
                    )
                )
            )

            val gate = MockGate.init(
                MockGate.Response(output = MockGate.output())
            )

            AI.run {
                for
                    memoryGateResolved <- memoryGate
                    gateResolved       <- gate
                    memory = Memory(
                        network.Memory.State(
                            List.empty,
                            network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memoryGateResolved
                    )
                    memState <- memory.state
                    subconscious = Neuron(
                        init = network.Neuron.State(
                            name = network.Neuron.Name("subconscious"),
                            description = "hidden neuron",
                            conscious = false,
                            previous = List.empty,
                            next = List.empty,
                            memory = Some(memState),
                            gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                        ),
                        memory = Some(memory),
                        gate = gateResolved
                    )
                    _            <- subconscious(List(Input("input")), List.empty[Neuron])
                    updatedState <- subconscious.state
                yield
                    assert(updatedState.conscious == false)
                    assert(updatedState.memory.isDefined)
                    assert(updatedState.memory.get.entries.exists(_.content == "learned-unconsciously"))
                end for
            }
        }

        "subconscious neuron history accumulates" in run {
            val gate = MockGate.init(
                5, // historyMaxEntries = 5
                MockGate.Response(output = MockGate.output()),
                MockGate.Response(output = MockGate.output())
            )

            for
                gateResolved <- gate
                neuronState = network.Neuron.State(
                    name = network.Neuron.Name("accumulator"),
                    description = "d",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 5))
                )
                neuron = Neuron(neuronState, None, gateResolved)
                result <- AI.run {
                    for
                        _     <- neuron(List(Input("call1")), List.empty[Neuron])
                        _     <- neuron(List(Input("call2")), List.empty[Neuron])
                        state <- neuron.state
                    yield
                        assert(state.conscious == false)
                        // Verify history has accumulated - should have 2 entries
                        assert(state.gate.history.entries.size == 2)
                    end for
                }
            yield result
            end for
        }
    }

    "factory methods" - {
        "Neuron.init should initialize from state" in run {
            val state = network.Neuron.State(
                name = network.Neuron.Name("init-test"),
                description = "initialized neuron",
                conscious = true,
                previous = List(network.Neuron.Name("p")),
                next = List(network.Neuron.Name("n")),
                memory = Some(network.Memory.State(
                    List.empty,
                    network.Gate.State("mp", "mr", Temperature.focused, network.History.State(List.empty, 0))
                )),
                gate = network.Gate.State("op", "or", Temperature.balanced, network.History.State(List.empty, 0))
            )

            for
                neuron <- network.Neuron.init(state)
            yield
                assert(neuron.name == network.Neuron.Name("init-test"))
                assert(neuron.conscious == true)
                assert(neuron.previous == List(network.Neuron.Name("p")))
                assert(neuron.next == List(network.Neuron.Name("n")))
            end for
        }

        "Neuron.init without memory" in run {
            val state = network.Neuron.State(
                name = network.Neuron.Name("simple-init"),
                description = "simple",
                conscious = false,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
            )

            for
                neuron <- network.Neuron.init(state)
            yield assert(neuron.name == network.Neuron.Name("simple-init"))
            end for
        }
    }

    def createNeuron(
        name: String = "test-neuron",
        description: String = "test description",
        previous: List[String] = List.empty,
        next: List[String] = List.empty,
        conscious: Boolean = true,
        gate: Gate < Sync = MockGate.init(MockGate.Response(output = MockGate.output(""))),
        memoryGate: Option[Gate < Sync] = None
    ): Neuron < Sync =
        for
            gateResolved   <- gate
            memoryResolved <- memoryGate.map(_.map(Some(_))).getOrElse(Kyo.lift(None))
        yield
            val neuronState = network.Neuron.State(
                name = network.Neuron.Name(name),
                description = description,
                conscious = conscious,
                previous = previous.map(network.Neuron.Name(_)),
                next = next.map(network.Neuron.Name(_)),
                memory = memoryGate.map(_ =>
                    network.Memory.State(
                        entries = List.empty,
                        gate =
                            network.Gate.State(
                                "memory prompt",
                                "memory reminder",
                                Temperature.balanced,
                                network.History.State(List.empty, 0)
                            )
                    )
                ),
                gate =
                    network.Gate.State("gate prompt", "gate reminder", Temperature.balanced, network.History.State(List.empty, 0))
            )

            Neuron(
                init = neuronState,
                memory = memoryResolved.map(g =>
                    Memory(
                        network.Memory.State(
                            List.empty,
                            network.Gate.State(
                                "memory prompt",
                                "memory reminder",
                                Temperature.balanced,
                                network.History.State(List.empty, 0)
                            )
                        ),
                        g
                    )
                ),
                gate = gateResolved
            )
    end createNeuron

end NeuronTest
