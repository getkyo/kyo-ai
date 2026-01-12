package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ren.*

class GateLiveTest extends Test:

    "basic string generation" in run {
        case class Acknowledgment(
            @doc("Acknowledgment message") message: String,
            @doc("Whether input was received") received: Boolean
        )

        val initialState = network.Gate.State(
            p"""You are a simple gate that processes input strings.
                Generate an Acknowledgment confirming receipt.""",
            p"""Generate Acknowledgment.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate   <- network.Gate.init(initialState)
                output <- gate[Acknowledgment](List(Input("hello world")))
            yield
                assert(output.message.nonEmpty, "Message should not be empty")
                assert(output.received, "Should acknowledge receipt")
            end for
        }
    }

    "typed case class generation" in run {
        case class Response(
            @doc("A greeting message") message: String,
            @doc("Whether the input was understood") understood: Boolean,
            @doc("Type of interaction: must be exactly 'greeting'") interactionType: String
        )

        val initialState = network.Gate.State(
            p"""You are a gate that generates structured responses.
                Analyze the input and produce a Response object.
                For greeting inputs, set interactionType to exactly 'greeting'.""",
            p"""Generate Response with all fields.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate     <- network.Gate.init(initialState)
                response <- gate[Response](List(Input("How are you?")))
            yield
                assert(response.message.nonEmpty, "Message should not be empty")
                assert(response.understood, "Should understand simple greeting")
                assert(response.interactionType.toLowerCase == "greeting", s"Should be 'greeting' but got: ${response.interactionType}")
            end for
        }
    }

    "uses context in generation" in run {
        case class ContextualAnswer(
            @doc("Answer derived from context") answer: String,
            @doc("The numerical value extracted from context") value: Int
        )

        val initialState = network.Gate.State(
            p"""You are a gate that incorporates context into your responses.
                Use both the input and context to generate a ContextualAnswer.
                Extract the numerical value from the context.""",
            p"""Generate ContextualAnswer.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate <- network.Gate.init(initialState)
                output <- gate[ContextualAnswer](
                    List(Input(List("What is the answer?"), List("Context: The answer to life is 42")))
                )
            yield
                assert(output.answer.nonEmpty, "Answer should not be empty")
                assert(output.value == 42, s"Should extract value 42 from context but got ${output.value}")
            end for
        }
    }

    "maintains history across calls" in run {
        case class NameRecall(
            @doc("The exact name that was remembered") name: String,
            @doc("Whether the name was found in history") foundInHistory: Boolean
        )

        val initialState = network.Gate.State(
            p"""You are a gate that maintains conversation history.
                When asked for a name, extract the exact name from previous messages.
                Return only the name itself in the name field.""",
            p"""Use history to recall exact information.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate   <- network.Gate.init(initialState)
                first  <- gate[String](List(Input("My name is Alice")))
                second <- gate[NameRecall](List(Input("What is my name?")))
            yield
                assert(second.name.toLowerCase == "alice", s"Should recall exact name 'Alice' but got: ${second.name}")
                assert(second.foundInHistory, "Should find name in conversation history")
            end for
        }
    }

    "respects temperature setting" in run {
        case class FocusedResponse(
            @doc("The response keyword") keyword: String,
            @doc("Consistency indicator") consistent: Boolean
        )

        val focusedState = network.Gate.State(
            p"""You are a deterministic gate with focused temperature.
                Generate a FocusedResponse with keyword set to exactly 'FOCUSED' and consistent set to true.""",
            p"""Generate FocusedResponse with exact values.""",
            Temperature.focused,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                focusedGate <- network.Gate.init(focusedState)
                focusedOut  <- focusedGate[FocusedResponse](List(Input("test")))
            yield
                assert(focusedOut.keyword.toUpperCase == "FOCUSED", s"Should be exactly 'FOCUSED' but got: ${focusedOut.keyword}")
                assert(focusedOut.consistent, "Should be consistent with focused temperature")
            end for
        }
    }

    "handles complex type generation" in run {
        case class Task(
            @doc("Task title") title: String,
            @doc("Task priority: high, medium, or low") priority: String,
            @doc("Estimated hours") estimatedHours: Int
        )

        val initialState = network.Gate.State(
            p"""You are a task planner gate.
                Analyze the input and generate a Task object with appropriate details.""",
            p"""Generate Task with title, priority, and estimatedHours.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate <- network.Gate.init(initialState)
                task <- gate[Task](List(Input("Build a REST API for user authentication")))
            yield
                assert(
                    task.title.toLowerCase.contains("api") || task.title.toLowerCase.contains("auth") || task.title.toLowerCase.contains(
                        "rest"
                    ),
                    s"Title should relate to REST API/authentication: ${task.title}"
                )
                assert(
                    Set("high", "medium", "low").contains(task.priority.toLowerCase),
                    s"Priority should be high/medium/low: ${task.priority}"
                )
                assert(task.estimatedHours > 0, "Estimated hours should be positive")
            end for
        }
    }

    "handles chunk types" in run {
        case class Item(
            @doc("Item name") name: String
        )

        val initialState = network.Gate.State(
            p"""You are a list generator gate.
                Generate a List of Item objects based on the input.
                Create between 2 and 5 items.""",
            p"""Generate List of Items.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate  <- network.Gate.init(initialState)
                items <- gate[List[Item]](List(Input("List programming languages")))
            yield
                assert(items.size >= 2 && items.size <= 5, s"Should generate 2-5 items but got ${items.size}")
                val knownLanguages =
                    Set("java", "python", "scala", "javascript", "kotlin", "rust", "go", "c++", "c#", "ruby", "php", "swift", "typescript")
                val languageMatches = items.filter(item =>
                    knownLanguages.exists(lang => item.name.toLowerCase == lang || item.name.toLowerCase.contains(lang))
                )
                assert(
                    languageMatches.size >= 2,
                    s"Should generate at least 2 known programming languages but got ${languageMatches.size}: ${items.map(_.name)}"
                )
            end for
        }
    }

    "state reflects history updates" in run {
        val initialState = network.Gate.State(
            p"""Process input and return a simple response.""",
            p"""Simple response.""",
            Temperature.balanced,
            network.History.State(List.empty, 5)
        )

        AI.run {
            for
                gate         <- network.Gate.init(initialState)
                initialState <- gate.state
                _            <- gate[String](List(Input("first")))
                afterFirst   <- gate.state
                _            <- gate[String](List(Input("second")))
                afterSecond  <- gate.state
            yield
                assert(initialState.history.entries.isEmpty, "Initial history should be empty")
                assert(
                    afterFirst.history.entries.size == 1,
                    s"After first call should have 1 entry but has ${afterFirst.history.entries.size}"
                )
                assert(
                    afterSecond.history.entries.size == 2,
                    s"After second call should have 2 entries but has ${afterSecond.history.entries.size}"
                )
            end for
        }
    }

    "limits history to maxEntries" in run {
        val initialState = network.Gate.State(
            p"""Process input and return acknowledgment.""",
            p"""Acknowledge input.""",
            Temperature.balanced,
            network.History.State(List.empty, 3)
        )

        AI.run {
            for
                gate  <- network.Gate.init(initialState)
                _     <- gate[String](List(Input("first")))
                _     <- gate[String](List(Input("second")))
                _     <- gate[String](List(Input("third")))
                _     <- gate[String](List(Input("fourth")))
                state <- gate.state
            yield
                assert(
                    state.history.entries.size == 3,
                    s"History should be limited to 3 entries but has ${state.history.entries.size}"
                )
                assert(
                    !state.history.entries.exists(_.inputs.head.tasks.head == "first"),
                    "Oldest entry should be dropped"
                )
            end for
        }
    }

    "handles multiple context strings" in run {
        case class FactSummary(
            @doc("Summary of all facts") summary: String,
            @doc("Number of facts processed: must be exactly 3") factCount: Int,
            @doc("Key colors mentioned: should include blue and green") colors: List[String]
        )

        val initialState = network.Gate.State(
            p"""You are a gate that processes multiple context items.
                Generate a FactSummary incorporating all context information.
                Count the exact number of facts provided.
                Extract all colors mentioned in the facts.""",
            p"""Generate FactSummary with accurate counts.""",
            Temperature.balanced,
            network.History.State(List.empty, 10)
        )

        AI.run {
            for
                gate <- network.Gate.init(initialState)
                output <- gate[FactSummary](
                    List(Input(
                        List("Summarize the information"),
                        List("Fact 1: Sky is blue", "Fact 2: Grass is green", "Fact 3: Sun is bright")
                    ))
                )
            yield
                assert(output.summary.nonEmpty, "Summary should not be empty")
                assert(output.factCount == 3, s"Should process exactly 3 facts but got ${output.factCount}")
                assert(output.colors.size >= 2, s"Should extract at least 2 colors but got ${output.colors.size}: ${output.colors}")
                val expectedColors = Set("blue", "green")
                val foundColors    = output.colors.map(_.toLowerCase).toSet
                assert(
                    expectedColors.subsetOf(foundColors),
                    s"Should extract 'blue' and 'green' but got: ${output.colors}"
                )
            end for
        }
    }

end GateLiveTest
