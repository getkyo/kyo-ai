package kyo.ren

import kyo.*
import kyo.ai.json.*
import kyo.ren.network.*

class MindLiveTest extends Test:

    "simple mind with single neuron" in run {
        case class Analysis(
            @doc("Key insight from the analysis") insight: String,
            @doc("Confidence level: high, medium, or low") confidence: String
        )

        val neuronState = network.Neuron.State(
            name = Neuron.Name("analyzer"),
            description = "Analyzes input",
            previous = List.empty,
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Analyze the input and generate a brief insight.""",
                p"""Generate insight.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "simple-analyzer",
            maxRecursions = 3,
            neurons = List(neuronState),
            output = network.Gate.State(
                p"""Based on the input and thoughts, generate an Analysis with insight and confidence level.""",
                p"""Generate Analysis.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind   <- Mind.init(mindState)
                result <- mind[Analysis](List(Input("Evaluate the importance of code reviews")))
            yield
                assert(result.insight.nonEmpty, "Insight should not be empty")
                assert(
                    Set("high", "medium", "low").contains(result.confidence.toLowerCase),
                    s"Confidence should be high/medium/low but got: ${result.confidence}"
                )
        }
    }

    "mind with network and structured output" in run {
        case class Recommendation(
            @doc("The recommended action") action: String,
            @doc("Priority level: 1 (highest) to 5 (lowest)") priority: Int,
            @doc("Supporting reasons") reasons: List[String]
        )

        val analyzerState = network.Neuron.State(
            name = Neuron.Name("analyzer"),
            description = "Analyzes the situation",
            previous = List.empty,
            next = List(Neuron.Name("recommender")),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Analyze the situation and identify key factors.""",
                p"""Analyze situation.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val recommenderState = network.Neuron.State(
            name = Neuron.Name("recommender"),
            description = "Makes recommendations",
            previous = List(Neuron.Name("analyzer")),
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Based on the analysis, generate a recommendation.""",
                p"""Generate recommendation.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "recommendation-mind",
            maxRecursions = 5,
            neurons = List(analyzerState, recommenderState),
            output = network.Gate.State(
                p"""Based on the input and network thoughts, generate a Recommendation.
                    Priority must be between 1 and 5.
                    Provide at least 2 reasons.""",
                p"""Generate Recommendation with all fields.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind   <- Mind.init(mindState)
                result <- mind[Recommendation](List(Input("Should we refactor the legacy authentication system?")))
            yield
                assert(result.action.nonEmpty, "Action should not be empty")
                assert(result.priority >= 1 && result.priority <= 5, s"Priority must be 1-5 but got: ${result.priority}")
                assert(result.reasons.size >= 2, s"Should have at least 2 reasons but got ${result.reasons.size}")
        }
    }

    "mind with memory integration" in run {
        case class Decision(
            @doc("The decision made") decision: String,
            @doc("Whether memory was consulted") usedMemory: Boolean,
            @doc("Key facts that influenced the decision") influencingFacts: List[String]
        )

        val neuronState = network.Neuron.State(
            name = Neuron.Name("decision-maker"),
            description = "Makes decisions using memory",
            previous = List.empty,
            next = List.empty,
            conscious = true,
            memory = Some(network.Memory.State(
                List(
                    network.Memory.Entry(0, 0, "Company policy: Security is the top priority"),
                    network.Memory.Entry(0, 0, "Company policy: User experience should not be compromised")
                ),
                network.Gate.State(
                    p"""Retrieve relevant memory entries for the decision.""",
                    p"""Retrieve memories.""",
                    Temperature.balanced,
                    network.History.State(List.empty, 0)
                )
            )),
            gate = network.Gate.State(
                p"""Make a decision considering the memory context.""",
                p"""Make decision with memory.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "decision-mind",
            maxRecursions = 3,
            neurons = List(neuronState),
            output = network.Gate.State(
                p"""Based on the input and thoughts, generate a Decision.
                    The neuron has memory, so set usedMemory to true.
                    Extract facts from the thoughts that influenced the decision.""",
                p"""Generate Decision with usedMemory=true.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind   <- Mind.init(mindState)
                result <- mind[Decision](List(Input("Should we implement two-factor authentication?")))
            yield
                assert(result.decision.nonEmpty, "Decision should not be empty")
                assert(result.usedMemory, "Should have consulted memory")
                assert(result.influencingFacts.nonEmpty, "Should have influencing facts")
        }
    }

    "mind respects maxRecursions limit" in run {
        case class Output(
            @doc("Output message") message: String,
            @doc("Number of processing steps") steps: Int
        )

        val n1State = network.Neuron.State(
            name = Neuron.Name("n1"),
            description = "Step 1",
            previous = List.empty,
            next = List(Neuron.Name("n2")),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Process step 1.""",
                p"""Step 1.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val n2State = network.Neuron.State(
            name = Neuron.Name("n2"),
            description = "Step 2",
            previous = List(Neuron.Name("n1")),
            next = List(Neuron.Name("n3")),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Process step 2.""",
                p"""Step 2.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val n3State = network.Neuron.State(
            name = Neuron.Name("n3"),
            description = "Step 3",
            previous = List(Neuron.Name("n2")),
            next = List(Neuron.Name("n4")),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Process step 3.""",
                p"""Step 3.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val n4State = network.Neuron.State(
            name = Neuron.Name("n4"),
            description = "Step 4",
            previous = List(Neuron.Name("n3")),
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Process final step.""",
                p"""Final step.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "limited-mind",
            maxRecursions = 2,
            neurons = List(n1State, n2State, n3State, n4State),
            output = network.Gate.State(
                p"""Generate an Output based on the thoughts received in the 'context' array.
                    The 'steps' field must be the exact count of entries in the 'context' array.
                    If context is empty ([]), then steps must be 0.
                    Do NOT count the input; only count the context entries.""",
                p"""Generate Output with steps = context.length.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind   <- Mind.init(mindState)
                result <- mind[Output](List(Input("test")))
            yield
                assert(result.message.nonEmpty, "Message should not be empty")
                assert(
                    result.steps == 0,
                    s"With maxRecursions=2, should not reach conscious neuron n4 in 4-step chain, so 0 thoughts but got ${result.steps}"
                )
        }
    }

    "mind with complex routing network" in run {
        case class RoutedResponse(
            @doc("Which specialist handled the request") specialist: String,
            @doc("The specialist's response") response: String
        )

        val routerState = network.Neuron.State(
            name = Neuron.Name("router"),
            description = "Routes to specialists",
            previous = List.empty,
            next = List("tech-specialist", "business-specialist").map(Neuron.Name(_)),
            conscious = false,
            memory = None,
            gate = network.Gate.State(
                p"""Route to the appropriate specialist.
                    For technical questions, activate tech-specialist.
                    For business questions, activate business-specialist.""",
                p"""Route to the appropriate specialists.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val techState = network.Neuron.State(
            name = Neuron.Name("tech-specialist"),
            description = "Handles technical questions",
            previous = List(Neuron.Name("router")),
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Provide a technical analysis. Your response must include the word 'implementation'.""",
                p"""Technical analysis.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val businessState = network.Neuron.State(
            name = Neuron.Name("business-specialist"),
            description = "Handles business questions",
            previous = List(Neuron.Name("router")),
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Provide a business analysis.""",
                p"""Business analysis.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "routing-mind",
            maxRecursions = 5,
            neurons = List(routerState, techState, businessState),
            output = network.Gate.State(
                p"""Based on the thoughts in the context, generate a RoutedResponse.
                    The 'specialist' field should be either 'tech-specialist' or 'business-specialist' depending on which specialist provided the thought.
                    The 'response' field must be the EXACT text from the specialist's thought in the context array - do NOT paraphrase, summarize, or reformulate it.
                    Copy the specialist's response verbatim.""",
                p"""Generate RoutedResponse with exact specialist response.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind   <- Mind.init(mindState)
                result <- mind[RoutedResponse](List(Input("How should we architect the microservices?")))
            yield
                assert(result.specialist.contains("tech"), s"Should route to tech specialist but got: ${result.specialist}")
                assert(result.response.nonEmpty, "Response should not be empty")
        }
    }

    "mind state persistence" in run {
        case class SimpleOutput(
            @doc("Output value") value: String
        )

        val neuronState = network.Neuron.State(
            name = Neuron.Name("processor"),
            description = "Processes input",
            previous = List.empty,
            next = List.empty,
            conscious = true,
            memory = None,
            gate = network.Gate.State(
                p"""Process the input.""",
                p"""Process.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mindState = Mind.State(
            name = "stateful-mind",
            maxRecursions = 3,
            neurons = List(neuronState),
            output = network.Gate.State(
                p"""Generate output.""",
                p"""Generate.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                mind           <- Mind.init(mindState)
                _              <- mind[SimpleOutput](List(Input("test")))
                retrievedState <- mind.state
            yield
                assert(retrievedState.name == "stateful-mind", s"Name should be preserved: ${retrievedState.name}")
                assert(retrievedState.maxRecursions == 3, s"maxRecursions should be preserved: ${retrievedState.maxRecursions}")
                assert(retrievedState.neurons.size == 1, s"Should have 1 neuron: ${retrievedState.neurons.size}")
        }
    }

end MindLiveTest
