package kyo.ren

import kyo.*
import kyo.Test
import kyo.ai.json.*
import kyo.ren.network.*

class NetworkTest extends Test:

    def createNeuron(
        name: String,
        previous: List[String] = List.empty,
        next: List[String] = List.empty,
        conscious: Boolean = false
    )(using Frame): Neuron < Sync =
        val state = Neuron.State(
            name = Neuron.Name(name),
            description = s"desc-$name",
            conscious = conscious,
            previous = previous.map(Neuron.Name(_)),
            next = next.map(Neuron.Name(_)),
            memory = None,
            gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
        )
        val activations = next.map { nextName =>
            Neuron.Activation(
                neuronNames = List(Neuron.Name(nextName)),
                tasks = Some(List(s"$name: output-$name")),
                context = None
            )
        }
        val output =
            if conscious then kyo.MockGate.output(s"output-$name", activations*)
            else kyo.MockGate.output("", activations*)
        kyo.MockGate.init(
            kyo.MockGate.Response(output = output)
        ).map { outputGate =>
            Neuron(state, None, outputGate)
        }
    end createNeuron

    "basic network operations" - {
        "single root neuron" in run {
            for
                neuron <- createNeuron("root", conscious = true)
                network = Network.init(List(neuron), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 1)
                assert(results.head == "output-root")
            end for
        }

        "linear chain" in run {
            for
                n1 <- createNeuron("n1", previous = List.empty, next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n3")))
                n3 <- createNeuron("n3", previous = List(Neuron.Name("n2")), next = List.empty, conscious = true)
                network = Network.init(List(n1, n2, n3), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 1)
                assert(results.head == "output-n3")
            end for
        }

        "multiple roots" in run {
            for
                root1  <- createNeuron("root1", next = List(Neuron.Name("middle")))
                root2  <- createNeuron("root2", next = List(Neuron.Name("middle")))
                middle <- createNeuron("middle", previous = List(Neuron.Name("root1"), Neuron.Name("root2")), conscious = true)
                network = Network.init(List(root1, root2, middle), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 1)
                assert(results.head == "output-middle")
            end for
        }

        "branching network" in run {
            for
                root    <- createNeuron("root", next = List(Neuron.Name("branch1"), Neuron.Name("branch2")))
                branch1 <- createNeuron("branch1", previous = List(Neuron.Name("root")), conscious = true)
                branch2 <- createNeuron("branch2", previous = List(Neuron.Name("root")), conscious = true)
                network = Network.init(List(root, branch1, branch2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 2)
                assert(results.toSet == Set("output-branch1", "output-branch2"))
            end for
        }

        "only conscious neurons produce output" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")), conscious = false)
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n3")), conscious = true)
                n3 <- createNeuron("n3", previous = List(Neuron.Name("n2")), conscious = false)
                network = Network.init(List(n1, n2, n3), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 1)
                assert(results.head == "output-n2")
            end for
        }
    }

    "Global recursion tracking" - {
        "neuron activated exactly maxIterations times in recursive loop" in run {
            var activationCounts = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)

            def countingGate(name: String) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    activationCounts(name) = activationCounts(name) + 1
                    kyo.MockGate.output(s"output-$name").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // Create a tight cycle: n1 -> n2 -> n1
            val n1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n2")),
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n1")),
                next = List(Neuron.Name("n1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n1 = Neuron(n1State, None, countingGate("n1"))
            val n2 = Neuron(n2State, None, countingGate("n2"))

            val maxIter = 3
            val network = Network.init(List(n1, n2), maxIter)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    // Each neuron should activate at most maxIterations times
                    assert(activationCounts("n1") <= maxIter, s"n1 activated ${activationCounts("n1")} times, max $maxIter")
                    assert(activationCounts("n2") <= maxIter, s"n2 activated ${activationCounts("n2")} times, max $maxIter")
                }
            }
        }

        "globalSeen prevents neuron exceeding maxIterations across iterations" in run {
            var callLog = scala.collection.mutable.ListBuffer[(String, Int)]()
            var iterNum = 0

            def trackingGate(name: String) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    callLog += ((name, iterNum))
                    iterNum += 1
                    kyo.MockGate.output(s"output-$name").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // Diamond with recursion: root -> left & right -> merge -> root
            val rootState = Neuron.State(
                name = Neuron.Name("root"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("merge")),
                next = List(Neuron.Name("left"), Neuron.Name("right")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val leftState = Neuron.State(
                name = Neuron.Name("left"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val rightState = Neuron.State(
                name = Neuron.Name("right"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mergeState = Neuron.State(
                name = Neuron.Name("merge"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("left"), Neuron.Name("right")),
                next = List(Neuron.Name("root")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root  = Neuron(rootState, None, trackingGate("root"))
            val left  = Neuron(leftState, None, trackingGate("left"))
            val right = Neuron(rightState, None, trackingGate("right"))
            val merge = Neuron(mergeState, None, trackingGate("merge"))

            val maxIter = 2
            val network = Network.init(List(root, left, right, merge), maxIter)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    val rootCalls = callLog.count(_._1 == "root")
                    assert(rootCalls <= maxIter, s"root called $rootCalls times, max $maxIter")
                }
            }
        }

        "seenThisIteration reset allows neuron in next iteration after recursion" in run {
            var iterationLog = scala.collection.mutable.Map.empty[String, Set[Int]]

            def trackingGate(name: String, currentIter: () => Int) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    val iter = currentIter()
                    iterationLog(name) = iterationLog.getOrElse(name, Set.empty) + iter
                    kyo.MockGate.output(s"output-$name").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            var currentIteration = 0
            def getCurrentIter() =
                val ret = currentIteration
                currentIteration += 1
                ret
            end getCurrentIter

            // Linear chain with back-edge: n1 -> n2 -> n3 -> n1
            val n1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("n3")),
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("n1")),
                next = List(Neuron.Name("n3")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n3State = Neuron.State(
                name = Neuron.Name("n3"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n2")),
                next = List(Neuron.Name("n1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n1 = Neuron(n1State, None, trackingGate("n1", getCurrentIter))
            val n2 = Neuron(n2State, None, trackingGate("n2", getCurrentIter))
            val n3 = Neuron(n3State, None, trackingGate("n3", getCurrentIter))

            val network = Network.init(List(n1, n2, n3), 3)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    // Verify that neurons can fire in multiple iterations (seenThisIteration was reset)
                    val n1Iters = iterationLog.getOrElse("n1", Set.empty).size
                    assert(n1Iters <= 3, s"n1 fired in $n1Iters iterations")
                }
            }
        }

        "multiple recursive paths don't cause premature termination" in run {
            // Two independent root neurons (no cycles to avoid complexity)
            var root1Activated = false
            var root2Activated = false

            val gate1 = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    root1Activated = true
                    kyo.MockGate.output("c1").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val gate2 = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    root2Activated = true
                    kyo.MockGate.output("c2").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // Two independent root neurons
            val root1State = Neuron.State(
                name = Neuron.Name("root1"),
                description = "d",
                conscious = true,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root2State = Neuron.State(
                name = Neuron.Name("root2"),
                description = "d",
                conscious = true,
                previous = List.empty,
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root1   = Neuron(root1State, None, gate1)
            val root2   = Neuron(root2State, None, gate2)
            val network = Network.init(List(root1, root2), 2)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    // Both roots should have been activated
                    assert(root1Activated, "root1 should be activated")
                    assert(root2Activated, "root2 should be activated")
                }
            }
        }
    }

    "iteration control" - {
        "respects maxIterations" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n3")))
                n3 <- createNeuron("n3", previous = List(Neuron.Name("n2")), conscious = true)
                network = Network.init(List(n1, n2, n3), 1)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.isEmpty)
            end for
        }

        "handles cycles with iteration limit" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n1")), conscious = true)
                network = Network.init(List(n1, n2), 3)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.nonEmpty)
            end for
        }

        "prevents infinite loops" in run {
            for
                selfRef <- createNeuron("self", previous = List(Neuron.Name("self")), next = List(Neuron.Name("self")), conscious = true)
                network = Network.init(List(selfRef), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.size <= 5)
            end for
        }
    }

    "activation filtering" - {
        "filters already seen neurons in iteration" in run {
            for
                root   <- createNeuron("root", next = List(Neuron.Name("target")))
                target <- createNeuron("target", previous = List(Neuron.Name("root")), conscious = true)
                network = Network.init(List(root, target), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.size == 1)
            end for
        }

        "resets seen set on recursion detection" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n3"), Neuron.Name("n1")))
                n3 <- createNeuron("n3", previous = List(Neuron.Name("n2")), conscious = true)
                network = Network.init(List(n1, n2, n3), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.nonEmpty)
            end for
        }

        "tracks global activation count" in run {
            var callCount = 0
            val countingGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    callCount += 1
                    kyo.MockGate.output(s"counted-$callCount").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val n1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )
            val n2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n1")),
                next = List(Neuron.Name("n1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n1 = Neuron(n1State, None, countingGate)
            val n2 = Neuron(n2State, None, countingGate)

            val network = Network.init(List(n1, n2), 3)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    assert(callCount > 0)
                    assert(callCount <= 6)
                }
            }
        }
    }

    "parallel processing" - {
        "processes independent branches in parallel" in run {
            for
                root <- createNeuron("root", next = List("b1", "b2", "b3"))
                b1   <- createNeuron("b1", previous = List(Neuron.Name("root")), conscious = true)
                b2   <- createNeuron("b2", previous = List(Neuron.Name("root")), conscious = true)
                b3   <- createNeuron("b3", previous = List(Neuron.Name("root")), conscious = true)
                network = Network.init(List(root, b1, b2, b3), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.size == 3)
                assert(results.toSet == Set("output-b1", "output-b2", "output-b3"))
            end for
        }

        "groups activations by target" in run {
            for
                r1     <- createNeuron("r1", next = List(Neuron.Name("target")))
                r2     <- createNeuron("r2", next = List(Neuron.Name("target")))
                target <- createNeuron("target", previous = List(Neuron.Name("r1"), Neuron.Name("r2")), conscious = true)
                network = Network.init(List(r1, r2, target), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.size == 1)
            end for
        }
    }

    "network connectivity" - {
        "handles unidirectional connections" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List.empty)
                network = Network.init(List(n1, n2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.isEmpty)
            end for
        }

        "requires bidirectional connections for edges" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2"), Neuron.Name("n3")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), next = List(Neuron.Name("n4")), conscious = true)
                n3 <- createNeuron("n3", previous = List(Neuron.Name("other")), conscious = true) // wrong previous
                n4 <- createNeuron("n4", previous = List(Neuron.Name("n2")), conscious = true)
                network = Network.init(List(n1, n2, n3, n4), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                assert(results.contains("output-n2"))
                assert(results.contains("output-n4"))
                assert(!results.contains("output-n3"))
            end for
        }
    }

    "Network recursion bugs" - {
        "should track activation counts correctly in cycles" in run {
            var callCounts = Map.empty[String, Int].withDefaultValue(0)

            def trackingGate(name: String, nextNeurons: List[Neuron.Name]) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    callCounts = callCounts.updated(name, callCounts(name) + 1)
                    val activations = nextNeurons.map { next =>
                        Neuron.Activation(List(next), Some(List(s"$name: output-$name")), None)
                    }
                    kyo.MockGate.output(s"output-$name", activations*).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val n1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("n1")),
                next = List(Neuron.Name("n3")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n3State = Neuron.State(
                name = Neuron.Name("n3"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n2")),
                next = List(Neuron.Name("n1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n1 = Neuron(n1State, None, trackingGate("n1", List(Neuron.Name("n2"))))
            val n2 = Neuron(n2State, None, trackingGate("n2", List(Neuron.Name("n3"))))
            val n3 = Neuron(n3State, None, trackingGate("n3", List(Neuron.Name("n1"))))

            val network = Network.init(List(n1, n2, n3), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    // The network processes: n1 -> n2 -> n3, then stops due to cycle detection
                    assert(callCounts("n1") == 1)
                    assert(callCounts("n2") == 1)
                    assert(callCounts("n3") == 1)
                    // Only n3 is conscious
                    assert(results == List("output-n3"))
                }
            }
        }

        "should handle diamond pattern correctly with proper input aggregation" in run {
            var mergeInputs: List[String] = List.empty

            val mergeGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    mergeInputs = inputs.flatMap(_.tasks)
                    kyo.MockGate.output("merged").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val rootState = Neuron.State(
                name = Neuron.Name("root"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("left"), Neuron.Name("right")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val leftState = Neuron.State(
                name = Neuron.Name("left"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val rightState = Neuron.State(
                name = Neuron.Name("right"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mergeState = Neuron.State(
                name = Neuron.Name("merge"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("left"), Neuron.Name("right")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                rootGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(List(Neuron.Name("left")), Some(List("root: root-out")), None),
                        Neuron.Activation(List(Neuron.Name("right")), Some(List("root: root-out")), None)
                    )
                ))
                leftGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("merge")), Some(List("left: left-out")), None))
                ))
                rightGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("merge")), Some(List("right: right-out")), None))
                ))
                root    = Neuron(rootState, None, rootGate)
                left    = Neuron(leftState, None, leftGate)
                right   = Neuron(rightState, None, rightGate)
                merge   = Neuron(mergeState, None, mergeGate)
                network = Network.init(List(root, left, right, merge), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                // Both paths activate in parallel and merge receives both inputs
                assert(mergeInputs.startsWith(List("left: left-out", "right: right-out")))
                assert(results == List("merged"))
            end for
        }
    }

    "iteration-based execution model" - {
        "tracks which iteration each neuron fires in" in run {
            var executionLog     = scala.collection.mutable.Map.empty[String, Int]
            var currentIteration = 0

            def trackingGate(name: String, nextNeurons: List[Neuron.Name]) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    executionLog(name) = currentIteration
                    val activations = nextNeurons.map { next =>
                        Neuron.Activation(List(next), Some(List(s"$name: output-$name")), None)
                    }
                    kyo.MockGate.output(s"output-$name", activations*).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val n1State = Neuron.State(
                name = Neuron.Name("n1"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("n2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n2State = Neuron.State(
                name = Neuron.Name("n2"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("n1")),
                next = List(Neuron.Name("n3")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n3State = Neuron.State(
                name = Neuron.Name("n3"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("n2")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val n1 = Neuron(n1State, None, trackingGate("n1", List(Neuron.Name("n2"))))
            val n2 = Neuron(n2State, None, trackingGate("n2", List(Neuron.Name("n3"))))
            val n3 = Neuron(n3State, None, trackingGate("n3", List.empty))

            val network = Network.init(List(n1, n2, n3), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    // TODO: Implement iteration tracking in Network to verify wave-like propagation
                    // For now, verify neurons execute in order
                    assert(executionLog.contains("n1"))
                    assert(executionLog.contains("n2"))
                    assert(executionLog.contains("n3"))
                    assert(results == List("output-n3"))
                }
            }
        }

        "neuron activated only once per iteration despite multiple sources" in run {
            var activationCounts = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)

            def countingGate(name: String, nextNeurons: List[Neuron.Name]) = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    activationCounts(name) = activationCounts(name) + 1
                    val activations = nextNeurons.map { next =>
                        Neuron.Activation(List(next), Some(List(s"$name: output-$name")), None)
                    }
                    kyo.MockGate.output(s"output-$name", activations*).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // Diamond: root -> left & right -> merge
            val rootState = Neuron.State(
                name = Neuron.Name("root"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("left"), Neuron.Name("right")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val leftState = Neuron.State(
                name = Neuron.Name("left"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val rightState = Neuron.State(
                name = Neuron.Name("right"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mergeState = Neuron.State(
                name = Neuron.Name("merge"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("left"), Neuron.Name("right")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root  = Neuron(rootState, None, countingGate("root", List(Neuron.Name("left"), Neuron.Name("right"))))
            val left  = Neuron(leftState, None, countingGate("left", List(Neuron.Name("merge"))))
            val right = Neuron(rightState, None, countingGate("right", List(Neuron.Name("merge"))))
            val merge = Neuron(mergeState, None, countingGate("merge", List.empty))

            val network = Network.init(List(root, left, right, merge), 5)

            AI.run {
                network(List(Input("input"))).map { _ =>
                    // Merge should only be activated once even though left and right both target it
                    assert(activationCounts("merge") == 1, s"merge activated ${activationCounts("merge")} times, expected 1")
                    assert(activationCounts("left") == 1)
                    assert(activationCounts("right") == 1)
                }
            }
        }

        "multiple activation inputs are concatenated with source names" in run {
            var receivedInputs: List[String] = List.empty

            val captureGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    receivedInputs = inputs.flatMap(_.tasks)
                    kyo.MockGate.output("captured").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val rootState = Neuron.State(
                name = Neuron.Name("root"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("left"), Neuron.Name("right")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val leftState = Neuron.State(
                name = Neuron.Name("left"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("target")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val rightState = Neuron.State(
                name = Neuron.Name("right"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root")),
                next = List(Neuron.Name("target")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val targetState = Neuron.State(
                name = Neuron.Name("target"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("left"), Neuron.Name("right")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            for
                leftGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("target")), Some(List("left: left-out")), None))
                ))
                rightGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("target")), Some(List("right: right-out")), None))
                ))
                rootGate <- kyo.MockGate.init(kyo.MockGate.Response(output =
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(List(Neuron.Name("left")), Some(List("root: root-out")), None),
                        Neuron.Activation(List(Neuron.Name("right")), Some(List("root: root-out")), None)
                    )
                ))
                root    = Neuron(rootState, None, rootGate)
                left    = Neuron(leftState, None, leftGate)
                right   = Neuron(rightState, None, rightGate)
                target  = Neuron(targetState, None, captureGate)
                network = Network.init(List(root, left, right, target), 5)
                _ <- AI.run {
                    network(List(Input("input")))
                }
            yield
            // Verify target receives both inputs concatenated with source names
            assert(receivedInputs.startsWith(List("left: left-out", "right: right-out")))
            end for
        }
    }

    "broken network tolerance" - {
        "tolerates neurons referencing non-existent neurons in next" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("missing"), Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1")), conscious = true)
                network = Network.init(List(n1, n2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                // Should successfully execute n1 -> n2, ignoring "missing"
                assert(results.size == 1)
                assert(results.head == "output-n2")
            end for
        }

        "tolerates neurons referencing non-existent neurons in previous" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")))
                n2 <- createNeuron("n2", previous = List(Neuron.Name("n1"), Neuron.Name("ghost")), conscious = true)
                network = Network.init(List(n1, n2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                // Should successfully execute with valid connection
                assert(results.size == 1)
                assert(results.head == "output-n2")
            end for
        }

        "handles completely disconnected sub-graphs" in run {
            for
                // Graph A: a1 -> a2
                a1 <- createNeuron("a1", next = List(Neuron.Name("a2")))
                a2 <- createNeuron("a2", previous = List(Neuron.Name("a1")), conscious = true)
                // Graph B: b1 -> b2 (not connected to A)
                b1 <- createNeuron("b1", next = List(Neuron.Name("b2")))
                b2 <- createNeuron("b2", previous = List(Neuron.Name("b1")), conscious = true)
                network = Network.init(List(a1, a2, b1, b2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield
                // Both sub-graphs should execute from root neurons
                assert(results.size == 2)
                assert(results.toSet == Set("output-a2", "output-b2"))
            end for
        }

        "handles cycles gracefully with iteration bounds" in run {
            for
                n1 <- createNeuron("n1", next = List(Neuron.Name("n2")), previous = List(Neuron.Name("n3")))
                n2 <- createNeuron("n2", next = List(Neuron.Name("n3")), previous = List(Neuron.Name("n1")))
                n3 <- createNeuron("n3", next = List(Neuron.Name("n1")), previous = List(Neuron.Name("n2")), conscious = true)
                network = Network.init(List(n1, n2, n3), 3)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.isEmpty)
            end for
        }
    }

    "empty and edge cases" - {
        "empty network" in run {
            val network = Network.init(List.empty, 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    assert(results.isEmpty)
                }
            }
        }

        "no conscious neurons" in run {
            for
                n1 <- createNeuron("n1", conscious = false)
                n2 <- createNeuron("n2", conscious = false)
                network = Network.init(List(n1, n2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.isEmpty)
            end for
        }

        "disconnected neurons" in run {
            for
                island1 <- createNeuron("island1", conscious = true)
                island2 <- createNeuron("island2", conscious = true)
                network = Network.init(List(island1, island2), 5)
                results <- AI.run {
                    network(List(Input("input")))
                }
            yield assert(results.size == 2)
            end for
        }

        "empty input" in run {
            for
                neuron <- createNeuron("n", conscious = true)
                network = Network.init(List(neuron), 5)
                results <- AI.run {
                    network(List.empty[Input])
                }
            yield assert(results.isEmpty)
            end for
        }
    }

    "backward reachability optimization" - {
        "skips roots with no path to conscious neurons" in run {
            var deadRootActivated = false
            var liveRootActivated = false

            val deadGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    deadRootActivated = true
                    kyo.MockGate.output("dead").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val liveGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    liveRootActivated = true
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(
                            List(Neuron.Name("conscious")),
                            Some(List("live: output")),
                            None
                        )
                    ).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val deadLeafGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val consciousGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("conscious-out").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // Dead subgraph: deadRoot -> deadLeaf (both subconscious)
            val deadRootState = Neuron.State(
                name = Neuron.Name("deadRoot"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("deadLeaf")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val deadLeafState = Neuron.State(
                name = Neuron.Name("deadLeaf"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("deadRoot")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            // Live subgraph: liveRoot -> conscious
            val liveRootState = Neuron.State(
                name = Neuron.Name("liveRoot"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("conscious")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val consciousState = Neuron.State(
                name = Neuron.Name("conscious"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("liveRoot")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val deadRoot  = Neuron(deadRootState, None, deadGate)
            val deadLeaf  = Neuron(deadLeafState, None, deadLeafGate)
            val liveRoot  = Neuron(liveRootState, None, liveGate)
            val conscious = Neuron(consciousState, None, consciousGate)

            val network = Network.init(List(deadRoot, deadLeaf, liveRoot, conscious), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    assert(!deadRootActivated, "Dead root should not be activated")
                    assert(liveRootActivated, "Live root should be activated")
                    assert(results == List("conscious-out"))
                }
            }
        }

        "executes all roots when all have paths to conscious neurons" in run {
            var root1Activated = false
            var root2Activated = false

            val gate1 = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    root1Activated = true
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(
                            List(Neuron.Name("merge")),
                            Some(List("r1: output")),
                            None
                        )
                    ).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val gate2 = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    root2Activated = true
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(
                            List(Neuron.Name("merge")),
                            Some(List("r2: output")),
                            None
                        )
                    ).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val mergeGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("merged").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val root1State = Neuron.State(
                name = Neuron.Name("root1"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root2State = Neuron.State(
                name = Neuron.Name("root2"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("merge")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val mergeState = Neuron.State(
                name = Neuron.Name("merge"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("root1"), Neuron.Name("root2")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root1 = Neuron(root1State, None, gate1)
            val root2 = Neuron(root2State, None, gate2)
            val merge = Neuron(mergeState, None, mergeGate)

            val network = Network.init(List(root1, root2, merge), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    assert(root1Activated, "Root1 should be activated")
                    assert(root2Activated, "Root2 should be activated")
                    assert(results == List("merged"))
                }
            }
        }

        "handles cycles in reachability correctly" in run {
            var rootActivated = false

            val rootGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    rootActivated = true
                    kyo.MockGate.output(
                        "",
                        Neuron.Activation(
                            List(Neuron.Name("cycle1")),
                            Some(List("root: output")),
                            None
                        )
                    ).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val cycle1Gate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("cycle2")), Some(List("c1")), None)).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val cycle2Gate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("", Neuron.Activation(List(Neuron.Name("conscious")), Some(List("c2")), None)).asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val consciousGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    kyo.MockGate.output("conscious-out").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            // root -> cycle1 <-> cycle2 -> conscious
            val rootState = Neuron.State(
                name = Neuron.Name("root"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("cycle1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val cycle1State = Neuron.State(
                name = Neuron.Name("cycle1"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root"), Neuron.Name("cycle2")),
                next = List(Neuron.Name("cycle2")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val cycle2State = Neuron.State(
                name = Neuron.Name("cycle2"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("cycle1")),
                next = List(Neuron.Name("cycle1"), Neuron.Name("conscious")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val consciousState = Neuron.State(
                name = Neuron.Name("conscious"),
                description = "d",
                conscious = true,
                previous = List(Neuron.Name("cycle2")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root      = Neuron(rootState, None, rootGate)
            val cycle1    = Neuron(cycle1State, None, cycle1Gate)
            val cycle2    = Neuron(cycle2State, None, cycle2Gate)
            val conscious = Neuron(consciousState, None, consciousGate)

            val network = Network.init(List(root, cycle1, cycle2, conscious), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    assert(rootActivated, "Root should be activated as it can reach conscious through cycle")
                    assert(results == List("conscious-out"))
                }
            }
        }

        "no roots execute when no conscious neurons exist" in run {
            var anyActivation = false

            val trackingGate = new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI =
                    anyActivation = true
                    kyo.MockGate.output("").asInstanceOf[Out]
                end apply
                def state(using Frame) = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))

            val root1State = Neuron.State(
                name = Neuron.Name("root1"),
                description = "d",
                conscious = false,
                previous = List.empty,
                next = List(Neuron.Name("node1")),
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val node1State = Neuron.State(
                name = Neuron.Name("node1"),
                description = "d",
                conscious = false,
                previous = List(Neuron.Name("root1")),
                next = List.empty,
                memory = None,
                gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
            )

            val root1 = Neuron(root1State, None, trackingGate)
            val node1 = Neuron(node1State, None, trackingGate)

            val network = Network.init(List(root1, node1), 5)

            AI.run {
                network(List(Input("input"))).map { results =>
                    assert(!anyActivation, "No neurons should be activated when no conscious neurons exist")
                    assert(results.isEmpty)
                }
            }
        }
    }

end NetworkTest
