package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ren.*

class MemoryLiveTest extends Test:

    "can add entries to empty memory" in run {
        val initialState = network.Memory.State(
            List.empty,
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    When adding entries, use the Add operation with appropriate afterIndex values.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _      <- memory(List(Input("Add three entries to the empty memory with content 'first', 'second', and 'third'.")))
                state  <- memory.state
            yield
                assert(state.entries.size == 3, s"Expected 3 entries but got ${state.entries.size}")
                assert(
                    state.entries.map(_.content) == List("first", "second", "third"),
                    s"Expected List(first, second, third) but got ${state.entries.map(_.content)}"
                )
            end for
        }
    }

    "understands afterIndex positioning" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "first"),
                network.Memory.Entry(0, 0, "third")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    When adding entries, use the Add operation with appropriate afterIndex values.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _ <- memory(
                    List(Input("Insert a new entry with content 'second' between 'first' and 'third'. It should appear after index 0."))
                )
                state <- memory.state
            yield
                assert(state.entries.size == 3)
                assert(
                    state.entries.map(_.content) == List("first", "second", "third"),
                    s"Expected entries in order: first, second, third but got: ${state.entries.map(_.content)}"
                )
            end for
        }
    }

    "handles None afterIndex to append to end" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "first"),
                network.Memory.Entry(0, 0, "second")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    To append to the end, use afterIndex: null in the Add operation.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _      <- memory(List(Input("Append a new entry with content 'last' to the end of memory.")))
                state  <- memory.state
            yield
                assert(state.entries.size == 3)
                assert(
                    state.entries.map(_.content).last == "last",
                    s"Expected last entry to be 'last' but got ${state.entries.map(_.content).last}"
                )
            end for
        }
    }

    "can replace entries" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "first"),
                network.Memory.Entry(0, 0, "old"),
                network.Memory.Entry(0, 0, "third")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    Use the Replace operation to modify entry content at a specific index.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _      <- memory(List(Input("Replace the entry at index 1 (currently 'old') with content 'new'.")))
                state  <- memory.state
            yield
                assert(state.entries.size == 3)
                assert(
                    state.entries.map(_.content)(1) == "new",
                    s"Expected entry at index 1 to be 'new' but got ${state.entries.map(_.content)(1)}"
                )
            end for
        }
    }

    "can remove entries" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "keep1"),
                network.Memory.Entry(0, 0, "remove"),
                network.Memory.Entry(0, 0, "keep2")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    Use the Remove operation to delete entries at specific indices.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _      <- memory(List(Input("Remove the entry at index 1 (content 'remove').")))
                state  <- memory.state
            yield
                assert(state.entries.size == 2)
                assert(
                    state.entries.map(_.content) == List("keep1", "keep2"),
                    s"Expected List(keep1, keep2) but got ${state.entries.map(_.content)}"
                )
            end for
        }
    }

    "can perform multi-step operations" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "a"),
                network.Memory.Entry(0, 0, "b"),
                network.Memory.Entry(0, 0, "c")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    You can combine multiple operations (Remove, Replace, Add) in a single MemoryOutput.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _ <- memory(List(Input(
                    """Do the following:
                       1. Remove the entry at index 1 (content 'b')
                       2. Add a new entry 'new' at the end
                       3. Replace the entry at index 0 with 'A'"""
                )))
                state <- memory.state
            yield
                assert(state.entries.size == 3, s"Expected 3 entries but got ${state.entries.size}")
                assert(
                    state.entries.map(_.content) == List("A", "c", "new"),
                    s"Expected exact order List(A, c, new) after operations but got: ${state.entries.map(_.content)}"
                )
            end for
        }
    }

    "handles out-of-bounds indices with clamping" in run {
        val initialState = network.Memory.State(
            List(
                network.Memory.Entry(0, 0, "first"),
                network.Memory.Entry(0, 0, "second")
            ),
            network.Gate.State(
                p"""Generate MemoryOutput with operations to manage memory entries based on the input.
                    Note: Out-of-bounds indices are automatically clamped to valid ranges.""",
                p"""Generate valid MemoryOutput with update and retrieve fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                memory <- network.Memory.init(initialState)
                _ <- memory(List(Input(
                    "Add a new entry with content 'clamped' after index 100. The system will clamp index 100 to the last valid index."
                )))
                state <- memory.state
            yield
                assert(state.entries.size == 3)
                assert(
                    state.entries.map(_.content).last == "clamped",
                    s"Expected last entry to be 'clamped' due to index clamping, but got ${state.entries.map(_.content).last}"
                )
            end for
        }
    }

end MemoryLiveTest
