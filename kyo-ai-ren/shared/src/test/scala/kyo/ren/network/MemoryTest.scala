package kyo.ren.network

import kyo.*
import kyo.MockGate
import kyo.Test
import kyo.ai.json.*
import kyo.ren.*

class MemoryTest extends Test:

    "retrieval" - {
        "should retrieve single entry" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, Some(List(network.Memory.Range(0, 0))))
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "content1"),
                            network.Memory.Entry(0, 0, "content2")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    memory(List(Input("input"))).map { retrieved =>
                        assert(retrieved.exists(_.content == "content1"))
                        assert(!retrieved.exists(_.content == "content2"))
                    }
                }
            yield result
            end for
        }

        "should retrieve multiple entries" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, Some(List(network.Memory.Range(0, 0), network.Memory.Range(2, 2))))
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    memory(List(Input("input"))).map { retrieved =>
                        assert(List("first", "third").forall(content => retrieved.exists(_.content == content)))
                    }
                }
            yield result
            end for
        }

        "should filter invalid indices" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            None,
                            Some(List(
                                network.Memory.Range(-1, -1),
                                network.Memory.Range(0, 0),
                                network.Memory.Range(5, 5),
                                network.Memory.Range(1, 1)
                            ))
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "a"),
                            network.Memory.Entry(0, 0, "b")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    memory(List(Input("test"))).map { retrieved =>
                        assert(List("a", "b").forall(content => retrieved.exists(_.content == content)))
                    }
                }
            yield result
            end for
        }
    }

    "update operations" - {
        "should add entry after index" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(0), "new content"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries.map(_.content) == List("first", "new content", "second"))
                        assert(state.entries(1).reads == 0)
                }
            yield result
            end for
        }

        "should add multiple entries after same index" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(Some(0), "add1"),
                                network.Memory.Update.Add(Some(0), "add2")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(network.Memory.Entry(0, 0, "original")),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries.map(_.content) == List("original", "add1", "add2"))
                }
            yield result
            end for
        }

        "should replace entry content" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(1, "replaced"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(3, 0, "a"),
                            network.Memory.Entry(5, 0, "b"),
                            network.Memory.Entry(0, 0, "c")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries(1).content == "replaced")
                        assert(state.entries(1).reads == 5)
                }
            yield result
            end for
        }

        "should remove single entry" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Remove(List(1)))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "keep1"),
                            network.Memory.Entry(0, 0, "remove"),
                            network.Memory.Entry(0, 0, "keep2")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries.map(_.content) == List("keep1", "keep2"))
                }
            yield result
            end for
        }

        "should remove multiple entries" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Remove(List(0, 2, 3)))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "r1"),
                            network.Memory.Entry(0, 0, "keep1"),
                            network.Memory.Entry(0, 0, "r2"),
                            network.Memory.Entry(0, 0, "r3"),
                            network.Memory.Entry(0, 0, "keep2")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries.map(_.content) == List("keep1", "keep2"))
                }
            yield result
            end for
        }

        "should apply combined operations in order" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Remove(List(1)),
                                network.Memory.Update.Replace(2, "replaced"),
                                network.Memory.Update.Add(Some(0), "added")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "removed"),
                            network.Memory.Entry(0, 0, "third"),
                            network.Memory.Entry(0, 0, "fourth")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("first", "added", "replaced", "fourth"))
                }
            yield result
            end for
        }
    }

    "state management" - {
        "should update entries after operations" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, Some(List(network.Memory.Range(0, 0))))
                    )
                )
                initialEntry = network.Memory.Entry(0, 0, "initial")
                memory = Memory(
                    network.Memory.State(
                        entries = List(initialEntry),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 1)
                        assert(state.entries.head.reads == 1)
                        assert(state.entries.head.content == "initial")
                }
            yield result
            end for
        }
    }

    "gate context" - {
        "should pass entries as context to gate" in run {
            var capturedContext: Option[List[String]] = None

            val gate = new Gate:
                def apply[A: Json](inputs: List[Input])(using Frame): A < AI =
                    capturedContext = Some(inputs.map(i => Json.encode(i)))
                    network.Memory.MemoryOutput(None, None).asInstanceOf[A]

                def state(using Frame) = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))

            val entries = List(
                network.Memory.Entry(0, 0, "e1"),
                network.Memory.Entry(0, 0, "e2")
            )

            val memory = Memory(
                network.Memory.State(entries, network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))),
                gate
            )

            AI.run {
                memory(List(Input("test"))).map { _ =>
                    assert(capturedContext.isDefined)
                    // The last input contains the entries as JSON
                    val lastInput = capturedContext.get.last
                    assert(lastInput.contains("e1"))
                    assert(lastInput.contains("e2"))
                }
            }
        }
    }

    "empty memory" - {
        "should handle empty memory" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, None)
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List.empty,
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    memory(List(Input("input"))).map { retrieved =>
                        assert(retrieved.size == 0)
                    }
                }
            yield result
            end for
        }
    }

    "updates tracking" - {
        "should initialize with updates from state" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, None)
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 5, "content with 5 updates"),
                            network.Memory.Entry(0, 0, "never updated"),
                            network.Memory.Entry(0, 10, "content with 10 updates")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == 5)
                        assert(state.entries(1).updates == 0)
                        assert(state.entries(2).updates == 10)
                }
            yield result
            end for
        }

        "should increment updates on replace operation" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(1, "replaced content"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(5, 3, "second"),
                            network.Memory.Entry(0, 7, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == 0) // unchanged
                        assert(state.entries(1).updates == 4) // incremented from 3
                        assert(state.entries(1).content == "replaced content")
                        assert(state.entries(2).updates == 7) // unchanged
                }
            yield result
            end for
        }

        "should preserve updates for non-replaced entries" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(0, "new first"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(2, 1, "old first"),
                            network.Memory.Entry(3, 8, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("test")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == 2) // incremented
                        assert(state.entries(0).content == "new first")
                        assert(state.entries(1).updates == 8) // unchanged
                        assert(state.entries(1).content == "second")
                }
            yield result
            end for
        }

        "should handle multiple replaces in single operation" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Replace(0, "new-0"),
                                network.Memory.Update.Replace(2, "new-2")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 2, "a"),
                            network.Memory.Entry(0, 5, "b"),
                            network.Memory.Entry(0, 0, "c")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == 3) // 2 + 1
                        assert(state.entries(0).content == "new-0")
                        assert(state.entries(1).updates == 5) // unchanged
                        assert(state.entries(1).content == "b")
                        assert(state.entries(2).updates == 1) // 0 + 1
                        assert(state.entries(2).content == "new-2")
                }
            yield result
            end for
        }

        "newly added entries should have updates of 0" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(Some(0), "new entry 1"),
                                network.Memory.Update.Add(Some(1), "new entry 2")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(3, 7, "existing1"),
                            network.Memory.Entry(2, 4, "existing2")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries(0).content == "existing1")
                        assert(state.entries(0).updates == 7) // unchanged
                        assert(state.entries(1).content == "new entry 1")
                        assert(state.entries(1).updates == 0) // new entry
                        assert(state.entries(2).content == "existing2")
                        assert(state.entries(2).updates == 4) // unchanged
                        assert(state.entries(3).content == "new entry 2")
                        assert(state.entries(3).updates == 0) // new entry
                }
            yield result
            end for
        }

        "removed entries should not affect other entries' updates" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Remove(List(1, 3)))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(1, 2, "keep1"),
                            network.Memory.Entry(2, 3, "remove1"),
                            network.Memory.Entry(3, 4, "keep2"),
                            network.Memory.Entry(4, 5, "remove2"),
                            network.Memory.Entry(5, 6, "keep3")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries(0).content == "keep1")
                        assert(state.entries(0).updates == 2)
                        assert(state.entries(1).content == "keep2")
                        assert(state.entries(1).updates == 4)
                        assert(state.entries(2).content == "keep3")
                        assert(state.entries(2).updates == 6)
                }
            yield result
            end for
        }

        "should handle combined operations correctly" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Remove(List(1)),
                                network.Memory.Update.Replace(2, "replaced third"),
                                network.Memory.Update.Add(Some(0), "added after first")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 1, "first"),
                            network.Memory.Entry(0, 2, "second-removed"),
                            network.Memory.Entry(0, 3, "third"),
                            network.Memory.Entry(0, 4, "fourth")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries(0).content == "first")
                        assert(state.entries(0).updates == 1) // unchanged
                        assert(state.entries(1).content == "added after first")
                        assert(state.entries(1).updates == 0) // new
                        assert(state.entries(2).content == "replaced third")
                        assert(state.entries(2).updates == 4) // incremented from 3
                        assert(state.entries(3).content == "fourth")
                        assert(state.entries(3).updates == 4) // unchanged
                }
            yield result
            end for
        }

        "should preserve all fields including updates in state round-trip" in run {
            val originalState = network.Memory.State(
                entries = List(
                    network.Memory.Entry(1, 2, "a"),
                    network.Memory.Entry(3, 4, "b"),
                    network.Memory.Entry(5, 6, "c")
                ),
                gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
            )

            for
                memory         <- network.Memory.init(originalState)
                retrievedState <- memory.state
            yield
                retrievedState.entries.zipWithIndex.foreach { case (entry, idx) =>
                    val original = originalState.entries(idx)
                    assert(entry.reads == original.reads)
                    assert(entry.updates == original.updates)
                    assert(entry.content == original.content)
                }
                assert(retrievedState.entries.size == 3)
            end for
        }

        "should handle edge case of zero updates" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(0, "updated"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "original")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == 1) // 0 + 1
                        assert(state.entries(0).content == "updated")
                }
            yield result
            end for
        }

        "should handle large updates values" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(0, "updated"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, Int.MaxValue - 1, "content")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).updates == Int.MaxValue)
                        assert(state.entries(0).content == "updated")
                }
            yield result
            end for
        }
    }

    "Memory operation actual behavior" - {
        "operations apply to original indices, not sequentially" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(Some(0), "new"),
                                network.Memory.Update.Remove(List(1))
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries.map(_.content) == List("first", "new"))
                }
            yield result
            end for
        }

        "Replace takes precedence over Remove for same index" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Replace(0, "replaced"),
                                network.Memory.Update.Remove(List(0))
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "original"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries(0).content == "replaced")
                        assert(state.entries(1).content == "second")
                }
            yield result
            end for
        }

        "demonstrates the operation precedence issue" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Remove(List(0)),
                                network.Memory.Update.Add(Some(0), "should be at index 0")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries.map(_.content) == List("second", "third"))
                }
            yield result
            end for
        }
    }

    "reads tracking" - {
        "reads increments on each retrieval" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, Some(List(network.Memory.Range(0, 0))))
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List(network.Memory.Entry(0, 0, "content")),
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _      <- memory(List(Input("retrieve1")))
                        state1 <- memory.state
                        _      <- memory(List(Input("retrieve2")))
                        state2 <- memory.state
                        _      <- memory(List(Input("retrieve3")))
                        state3 <- memory.state
                    yield
                        assert(state1.entries(0).reads == 1)
                        assert(state2.entries(0).reads == 2)
                        assert(state3.entries(0).reads == 3)
                    end for
                }
            yield result
            end for
        }

        "reads only increments for retrieved entries" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(None, Some(List(network.Memory.Range(0, 0))))
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List(
                            network.Memory.Entry(0, 0, "retrieved"),
                            network.Memory.Entry(0, 0, "not-retrieved")
                        ),
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("call")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).reads == 1)
                        assert(state.entries(1).reads == 0)
                    end for
                }
            yield result
            end for
        }

        "reads increments for multiple entries" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            None,
                            Some(List(network.Memory.Range(0, 0), network.Memory.Range(2, 2)))
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List(
                            network.Memory.Entry(5, 0, "a"),
                            network.Memory.Entry(0, 0, "b"),
                            network.Memory.Entry(3, 0, "c")
                        ),
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("call")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).reads == 6)
                        assert(state.entries(1).reads == 0)
                        assert(state.entries(2).reads == 4)
                    end for
                }
            yield result
            end for
        }

        "reads preserved during updates" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Replace(0, "updated"))),
                            Some(List(network.Memory.Range(0, 0)))
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List(network.Memory.Entry(10, 5, "original")),
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("call")))
                        state <- memory.state
                    yield
                        assert(state.entries(0).reads == 11)
                        assert(state.entries(0).updates == 6)
                        assert(state.entries(0).content == "updated")
                    end for
                }
            yield result
            end for
        }

        "newly added entries have zero reads" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(0), "new"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        List(network.Memory.Entry(3, 5, "existing")),
                        network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("call")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries(0).reads == 3)
                        assert(state.entries(1).reads == 0)
                        assert(state.entries(1).content == "new")
                    end for
                }
            yield result
            end for
        }
    }

    "factory method" - {
        "Memory.init should create from state" in run {
            val state = network.Memory.State(
                entries = List(network.Memory.Entry(3, 3, "test")),
                gate = network.Gate.State("prompt", "reminder", Temperature.focused, network.History.State(List.empty, 10))
            )

            for
                memory       <- network.Memory.init(state)
                currentState <- memory.state
            yield
                assert(currentState.entries == state.entries)
                assert(currentState.gate == state.gate)
            end for
        }
    }

    "afterIndex Option handling" - {
        "should append to end when afterIndex is None" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(None, "appended"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("first", "second", "third", "appended"))
                }
            yield result
            end for
        }

        "should append to end when afterIndex is None on empty memory" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(None, "first entry"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List.empty,
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 1)
                        assert(state.entries.head.content == "first entry")
                }
            yield result
            end for
        }

        "should append multiple entries to end when afterIndex is None" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(None, "append1"),
                                network.Memory.Update.Add(None, "append2"),
                                network.Memory.Update.Add(None, "append3")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "existing")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("existing", "append1", "append2", "append3"))
                }
            yield result
            end for
        }

        "should handle mix of None and Some afterIndex" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(Some(0), "after-first"),
                                network.Memory.Update.Add(None, "at-end"),
                                network.Memory.Update.Add(Some(1), "after-second")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        // None resolves to lastIdx (1), so both "at-end" and "after-second" go after index 1
                        // They appear in the order they were specified in the ops chunk
                        assert(state.entries.size == 5)
                        assert(state.entries.map(_.content) == List("first", "after-first", "second", "at-end", "after-second"))
                }
            yield result
            end for
        }
    }

    "afterIndex clamping behavior" - {
        "should clamp negative afterIndex to 0" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(-1), "clamped"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries.map(_.content) == List("first", "clamped", "second"))
                }
            yield result
            end for
        }

        "should clamp large negative afterIndex to 0" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(-100), "clamped"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "a"),
                            network.Memory.Entry(0, 0, "b"),
                            network.Memory.Entry(0, 0, "c")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("a", "clamped", "b", "c"))
                }
            yield result
            end for
        }

        "should clamp afterIndex exceeding lastIdx to lastIdx" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(10), "clamped"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("first", "second", "third", "clamped"))
                }
            yield result
            end for
        }

        "should handle afterIndex equal to lastIdx" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(2), "at-last"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("first", "second", "third", "at-last"))
                }
            yield result
            end for
        }

        "should clamp afterIndex to 0 in single-entry memory" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(-5), "clamped"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "only")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 2)
                        assert(state.entries.map(_.content) == List("only", "clamped"))
                }
            yield result
            end for
        }

        "should handle afterIndex 0 explicitly" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(0), "at-zero"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries.map(_.content) == List("first", "at-zero", "second"))
                }
            yield result
            end for
        }

        "should clamp multiple operations with various out-of-bounds indices" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(
                                network.Memory.Update.Add(Some(-10), "negative"),
                                network.Memory.Update.Add(Some(100), "too-large"),
                                network.Memory.Update.Add(Some(1), "valid")
                            )),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        // -10 clamps to 0, 100 clamps to 1 (lastIdx), 1 stays at 1
                        // Both "too-large" and "valid" go after index 1 in the order specified
                        assert(state.entries.size == 5)
                        assert(state.entries.map(_.content) == List("first", "negative", "second", "too-large", "valid"))
                }
            yield result
            end for
        }

        "should handle Int.MinValue as afterIndex" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(Int.MinValue), "extreme-min"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 3)
                        assert(state.entries.map(_.content) == List("first", "extreme-min", "second"))
                }
            yield result
            end for
        }

        "should handle Int.MaxValue as afterIndex" in run {
            for
                gate <- MockGate.init(
                    MockGate.Response(
                        output = network.Memory.MemoryOutput(
                            Some(List(network.Memory.Update.Add(Some(Int.MaxValue), "extreme-max"))),
                            None
                        )
                    )
                )
                memory = Memory(
                    network.Memory.State(
                        entries = List(
                            network.Memory.Entry(0, 0, "first"),
                            network.Memory.Entry(0, 0, "second"),
                            network.Memory.Entry(0, 0, "third")
                        ),
                        gate = network.Gate.State("p", "r", Temperature.balanced, network.History.State(List.empty, 0))
                    ),
                    gate
                )
                result <- AI.run {
                    for
                        _     <- memory(List(Input("input")))
                        state <- memory.state
                    yield
                        assert(state.entries.size == 4)
                        assert(state.entries.map(_.content) == List("first", "second", "third", "extreme-max"))
                }
            yield result
            end for
        }
    }

end MemoryTest
