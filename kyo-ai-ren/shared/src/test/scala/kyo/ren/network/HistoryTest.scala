package kyo.ren.network

import kyo.*
import kyo.Test
import kyo.ren.*

class HistoryTest extends Test:

    "should initialize with given state" in run {
        val initialEntries = List(
            network.History.Entry(0, List(Input("input1")), "output1"),
            network.History.Entry(1, List(Input("input2")), "output2")
        )
        val state = network.History.State(initialEntries, maxEntries = 10)

        for
            history <- network.History.init(state)
            result  <- history.state
        yield assert(state == result)
        end for
    }

    "should append new entries" in run {
        val state = network.History.State(List.empty, maxEntries = 10)

        for
            history <- network.History.init(state)
            _       <- history(List(Input("test input")), "test output")
            result  <- history.state
        yield
            assert(result.entries.size == 1)
            assert(result.entries.head.inputs.head.tasks == List("test input"))
            assert(result.entries.head.output == "test output")
        end for
    }

    "should respect maxEntries limit" in run {
        val state = network.History.State(List.empty, maxEntries = 3)

        for
            history <- network.History.init(state)
            _       <- history(List(Input("input1")), "output1")
            _       <- history(List(Input("input2")), "output2")
            _       <- history(List(Input("input3")), "output3")
            _       <- history(List(Input("input4")), "output4")
            result  <- history.state
        yield
            assert(result.entries.size == 3)
            assert(result.entries.map(_.output) == List("output2", "output3", "output4"))
        end for
    }

    "should preserve entries order (oldest first)" in run {
        val state = network.History.State(List.empty, maxEntries = 5)

        for
            history <- network.History.init(state)
            _       <- history(List(Input("input1")), "output1")
            _       <- history(List(Input("input2")), "output2")
            _       <- history(List(Input("input3")), "output3")
            result  <- history.state
        yield
            val outputs = result.entries.map(_.output)
            assert(outputs == List("output1", "output2", "output3"))
        end for
    }

end HistoryTest
