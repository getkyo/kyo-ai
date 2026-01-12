package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ren.*

class NetworkLiveTest extends Test:

    "linear thought chain with memory" in run {
        val n1State = network.Neuron.State(
            name = Neuron.Name("analyzer"),
            description = "Analyzes input and extracts key information",
            conscious = false,
            previous = List.empty,
            next = List(Neuron.Name("memory-processor")),
            memory = None,
            gate = network.Gate.State(
                p"""Extract the main topic and key facts from the input.
                    Return a concise summary of what's important.""",
                p"""Extract and summarize key information.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val n2State = network.Neuron.State(
            name = Neuron.Name("memory-processor"),
            description = "Stores and recalls relevant facts",
            conscious = true,
            previous = List(Neuron.Name("analyzer")),
            next = List(Neuron.Name("synthesizer")),
            memory = Some(network.Memory.State(
                List(
                    network.Memory.Entry(0, 0, "Water boils at exactly 100 degrees Celsius")
                ),
                network.Gate.State(
                    p"""Retrieve relevant memory entries for the current analysis.""",
                    p"""Retrieve relevant memories.""",
                    Temperature.balanced,
                    network.History.State(List.empty, 0)
                )
            )),
            gate = network.Gate.State(
                p"""Process the analysis with memory context and produce a thought.
                    Your response must include the exact temperature value from memory.""",
                p"""Generate thought with exact temperature.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val n3State = network.Neuron.State(
            name = Neuron.Name("synthesizer"),
            description = "Synthesizes final conclusion",
            conscious = true,
            previous = List(Neuron.Name("memory-processor")),
            next = List.empty,
            memory = None,
            gate = network.Gate.State(
                p"""Synthesize a final conclusion based on the previous thought.""",
                p"""Synthesize conclusion.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                n1 <- network.Neuron.init(n1State)
                n2 <- network.Neuron.init(n2State)
                n3 <- network.Neuron.init(n3State)
                net = network.Network.init(List(n1, n2, n3), 5)
                thoughts <- net(List(Input("What temperature does water boil at?")))
            yield
                assert(thoughts.size == 2, s"Should have 2 thoughts from conscious neurons but got ${thoughts.size}")
                assert(
                    thoughts.exists(t => t.contains("100")),
                    s"Should include exact temperature '100' from memory: ${thoughts}"
                )
        }
    }

    "parallel analysis with selective routing" in run {
        val routerState = network.Neuron.State(
            name = Neuron.Name("router"),
            description = "Routes questions to appropriate specialist",
            conscious = false,
            previous = List.empty,
            next = List(Neuron.Name("math-specialist"), Neuron.Name("language-specialist")),
            memory = None,
            gate = network.Gate.State(
                p"""Pass through the input question exactly as received.""",
                p"""Pass through question.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val mathState = network.Neuron.State(
            name = Neuron.Name("math-specialist"),
            description = "Handles mathematical questions and calculations",
            conscious = true,
            previous = List(Neuron.Name("router")),
            next = List.empty,
            memory = None,
            gate = network.Gate.State(
                p"""Solve the mathematical question and provide the exact numerical answer.""",
                p"""Provide exact numerical answer.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val langState = network.Neuron.State(
            name = Neuron.Name("language-specialist"),
            description = "Handles language and grammar questions",
            conscious = true,
            previous = List(Neuron.Name("router")),
            next = List.empty,
            memory = None,
            gate = network.Gate.State(
                p"""Provide a linguistic analysis or explanation.""",
                p"""Provide language analysis.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                router <- network.Neuron.init(routerState)
                math   <- network.Neuron.init(mathState)
                lang   <- network.Neuron.init(langState)
                net = network.Network.init(List(router, math, lang), 5)
                thoughts <- net(List(Input("What is 15 multiplied by 3?")))
            yield
                assert(thoughts.size >= 1, s"Should route to at least one specialist but got ${thoughts.size}")
                assert(
                    thoughts.exists(t => t.contains("45")),
                    s"Should contain exact answer '45': ${thoughts}"
                )
        }
    }

    "diamond pattern with information aggregation" in run {
        val rootState = network.Neuron.State(
            Neuron.Name("root"),
            "Initial processor",
            false,
            List.empty,
            List(Neuron.Name("syntax-analyzer"), Neuron.Name("semantic-analyzer")),
            None,
            network.Gate.State(
                p"""Process the input sentence for analysis.""",
                p"""Process input.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val syntaxState = network.Neuron.State(
            Neuron.Name("syntax-analyzer"),
            "Analyzes sentence structure",
            false,
            List(Neuron.Name("root")),
            List(Neuron.Name("synthesizer")),
            None,
            network.Gate.State(
                p"""Analyze the grammatical structure of the sentence.""",
                p"""Analyze syntax.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val semanticState = network.Neuron.State(
            Neuron.Name("semantic-analyzer"),
            "Analyzes meaning",
            false,
            List(Neuron.Name("root")),
            List(Neuron.Name("synthesizer")),
            None,
            network.Gate.State(
                p"""Analyze the meaning and intent of the sentence.""",
                p"""Analyze semantics.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val synthState = network.Neuron.State(
            Neuron.Name("synthesizer"),
            "Combines analyses",
            true,
            List(Neuron.Name("syntax-analyzer"), Neuron.Name("semantic-analyzer")),
            List.empty,
            None,
            network.Gate.State(
                p"""Combine the syntax and semantic analyses into a unified insight.
                    The input contains analyses from both perspectives.
                    State that you received input from both analyzers.""",
                p"""Synthesize unified insight mentioning both inputs.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                root     <- network.Neuron.init(rootState)
                syntax   <- network.Neuron.init(syntaxState)
                semantic <- network.Neuron.init(semanticState)
                synth    <- network.Neuron.init(synthState)
                net = network.Network.init(List(root, syntax, semantic, synth), 5)
                thoughts <- net(List(Input("The cat sat on the mat")))
            yield
                assert(thoughts.size == 1, s"Should produce 1 unified thought but got ${thoughts.size}")
                assert(
                    thoughts.head.nonEmpty,
                    s"Thought should not be empty: ${thoughts.head}"
                )
        }
    }

    "recursive refinement with cycle detection" in run {
        val analyzerState = network.Neuron.State(
            Neuron.Name("analyzer"),
            "Analyzes input and produces draft answer",
            false,
            List.empty,
            List(Neuron.Name("critic")),
            None,
            network.Gate.State(
                p"""Analyze the question and produce a draft answer.
                    If this is a refinement, improve upon the previous attempt.""",
                p"""Produce draft answer.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val criticState = network.Neuron.State(
            Neuron.Name("critic"),
            "Evaluates analysis quality",
            false,
            List(Neuron.Name("analyzer")),
            List(Neuron.Name("analyzer"), Neuron.Name("finalizer")),
            None,
            network.Gate.State(
                p"""Evaluate the analysis quality.""",
                p"""Evaluate quality.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val finalizerState = network.Neuron.State(
            Neuron.Name("finalizer"),
            "Produces final output",
            true,
            List(Neuron.Name("critic")),
            List.empty,
            None,
            network.Gate.State(
                p"""Produce the final refined answer.
                    Your answer must mention that plants convert light into energy.""",
                p"""Finalize answer about light and energy.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                analyzer  <- network.Neuron.init(analyzerState)
                critic    <- network.Neuron.init(criticState)
                finalizer <- network.Neuron.init(finalizerState)
                net = network.Network.init(List(analyzer, critic, finalizer), 5)
                thoughts <- net(List(Input("Explain photosynthesis briefly")))
            yield
                assert(thoughts.size >= 1, s"Should produce at least 1 final thought but got ${thoughts.size}")
                assert(
                    thoughts.head.toLowerCase.contains("light") && thoughts.head.toLowerCase.contains("energy"),
                    s"Thought must mention both 'light' and 'energy': ${thoughts.head}"
                )
        }
    }

    "multi-level consciousness hierarchy" in run {
        val extractor1State = network.Neuron.State(
            Neuron.Name("extractor1"),
            "Extracts feature 1",
            false,
            List.empty,
            List(Neuron.Name("processor1")),
            None,
            network.Gate.State(
                p"""Extract key nouns from the input.""",
                p"""Extract nouns.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        val extractor2State = network.Neuron.State(
            Neuron.Name("extractor2"),
            "Extracts feature 2",
            false,
            List.empty,
            List(Neuron.Name("processor2")),
            None,
            network.Gate.State(
                p"""Extract key verbs and actions from the input.""",
                p"""Extract verbs.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        val processor1State = network.Neuron.State(
            Neuron.Name("processor1"),
            "Processes nouns",
            false,
            List(Neuron.Name("extractor1")),
            List(Neuron.Name("decision1")),
            None,
            network.Gate.State(
                p"""Process the extracted nouns.""",
                p"""Process nouns.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        val processor2State = network.Neuron.State(
            Neuron.Name("processor2"),
            "Processes verbs",
            false,
            List(Neuron.Name("extractor2")),
            List(Neuron.Name("decision2")),
            None,
            network.Gate.State(
                p"""Process the extracted verbs.""",
                p"""Process verbs.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        val decision1State = network.Neuron.State(
            Neuron.Name("decision1"),
            "Makes decision about entities",
            true,
            List(Neuron.Name("processor1")),
            List.empty,
            None,
            network.Gate.State(
                p"""Make a decision about the entities involved.
                    IMPORTANT: You must provide a consciousOutput with your decision.""",
                p"""Decide about entities with conscious output.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        val decision2State = network.Neuron.State(
            Neuron.Name("decision2"),
            "Makes decision about actions",
            true,
            List(Neuron.Name("processor2")),
            List.empty,
            None,
            network.Gate.State(
                p"""Make a decision about the actions taking place.
                    IMPORTANT: You must provide a consciousOutput with your decision.""",
                p"""Decide about actions with conscious output.""",
                Temperature.precise,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                e1 <- network.Neuron.init(extractor1State)
                e2 <- network.Neuron.init(extractor2State)
                p1 <- network.Neuron.init(processor1State)
                p2 <- network.Neuron.init(processor2State)
                d1 <- network.Neuron.init(decision1State)
                d2 <- network.Neuron.init(decision2State)
                net = network.Network.init(List(e1, e2, p1, p2, d1, d2), 5)
                thoughts <- net(List(Input("The dog chased the ball quickly")))
            yield assert(thoughts.size == 2, s"Should collect thoughts only from 2 conscious neurons but got ${thoughts.size}")
        }
    }

    "dynamic routing with memory-based decisions" in run {
        val routerState = network.Neuron.State(
            Neuron.Name("router"),
            "Routes based on user preferences",
            false,
            List.empty,
            List(Neuron.Name("action-read"), Neuron.Name("action-write"), Neuron.Name("action-delete")),
            Some(network.Memory.State(
                List(
                    network.Memory.Entry(0, 0, "user prefers reading"),
                    network.Memory.Entry(0, 0, "user dislikes deletion")
                ),
                network.Gate.State(
                    p"""Retrieve user preferences relevant to the requested action.""",
                    p"""Retrieve preferences.""",
                    Temperature.balanced,
                    network.History.State(List.empty, 0)
                )
            )),
            network.Gate.State(
                p"""Analyze the user request with memory context.""",
                p"""Analyze request.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val readState = network.Neuron.State(
            Neuron.Name("action-read"),
            "Handles read operations",
            true,
            List(Neuron.Name("router")),
            List.empty,
            None,
            network.Gate.State(
                p"""Execute read operation and describe what was read.""",
                p"""Execute read.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val writeState = network.Neuron.State(
            Neuron.Name("action-write"),
            "Handles write operations",
            true,
            List(Neuron.Name("router")),
            List.empty,
            None,
            network.Gate.State(
                p"""Execute write operation and confirm what was written.""",
                p"""Execute write.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val deleteState = network.Neuron.State(
            Neuron.Name("action-delete"),
            "Handles delete operations",
            true,
            List(Neuron.Name("router")),
            List.empty,
            None,
            network.Gate.State(
                p"""Execute delete operation and confirm deletion.""",
                p"""Execute delete.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                router <- network.Neuron.init(routerState)
                read   <- network.Neuron.init(readState)
                write  <- network.Neuron.init(writeState)
                delete <- network.Neuron.init(deleteState)
                net = network.Network.init(List(router, read, write, delete), 5)
                thoughts <- net(List(Input("I want to view the document")))
            yield assert(thoughts.size >= 1, s"Should route to an action and produce thought but got ${thoughts.size}")
        }
    }

    "iteration limit respected in long chain" in run {
        val n1State = network.Neuron.State(
            Neuron.Name("n1"),
            "Step 1",
            false,
            List.empty,
            List(Neuron.Name("n2")),
            None,
            network.Gate.State(p"""Process step 1.""", p"""Step 1.""", Temperature.balanced, network.History.State(List.empty, 0))
        )

        val n2State = network.Neuron.State(
            Neuron.Name("n2"),
            "Step 2",
            false,
            List(Neuron.Name("n1")),
            List(Neuron.Name("n3")),
            None,
            network.Gate.State(p"""Process step 2.""", p"""Step 2.""", Temperature.balanced, network.History.State(List.empty, 0))
        )

        val n3State = network.Neuron.State(
            Neuron.Name("n3"),
            "Step 3",
            false,
            List(Neuron.Name("n2")),
            List(Neuron.Name("n4")),
            None,
            network.Gate.State(p"""Process step 3.""", p"""Step 3.""", Temperature.balanced, network.History.State(List.empty, 0))
        )

        val n4State = network.Neuron.State(
            Neuron.Name("n4"),
            "Step 4",
            true,
            List(Neuron.Name("n3")),
            List.empty,
            None,
            network.Gate.State(
                p"""Produce final output.""",
                p"""Final output.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                n1 <- network.Neuron.init(n1State)
                n2 <- network.Neuron.init(n2State)
                n3 <- network.Neuron.init(n3State)
                n4 <- network.Neuron.init(n4State)
                net = network.Network.init(List(n1, n2, n3, n4), 2)
                thoughts <- net(List(Input("test")))
            yield assert(
                thoughts.isEmpty,
                s"Should not reach n4 with maxIterations=2 in 4-step chain, but got ${thoughts.size} thoughts"
            )
        }
    }

    "handles disconnected neurons" in run {
        val connected1State = network.Neuron.State(
            Neuron.Name("connected1"),
            "Connected neuron 1",
            false,
            List.empty,
            List(Neuron.Name("connected2")),
            None,
            network.Gate.State(
                p"""Process in connected chain.""",
                p"""Connected processing.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val connected2State = network.Neuron.State(
            Neuron.Name("connected2"),
            "Connected neuron 2",
            true,
            List(Neuron.Name("connected1")),
            List.empty,
            None,
            network.Gate.State(
                p"""Produce thought from connected chain.""",
                p"""Connected thought.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        val islandState = network.Neuron.State(
            Neuron.Name("island"),
            "Isolated neuron",
            true,
            List(Neuron.Name("nonexistent")), // Make it truly disconnected - not a root
            List.empty,
            None,
            network.Gate.State(
                p"""Produce independent thought.""",
                p"""Independent thought.""",
                Temperature.balanced,
                network.History.State(List.empty, 0)
            )
        )

        AI.run {
            for
                c1     <- network.Neuron.init(connected1State)
                c2     <- network.Neuron.init(connected2State)
                island <- network.Neuron.init(islandState)
                net = network.Network.init(List(c1, c2, island), 5)
                thoughts <- net(List(Input("test input")))
            yield
            // Island neuron should not produce output because it's not reachable from roots
            // (it has a previous reference to a non-existent neuron)
            // connected2 might not produce consciousOutput depending on LLM response
            assert(thoughts.size <= 1, s"Should have at most 1 thought from connected chain, got ${thoughts.size}")
        }
    }

end NetworkLiveTest
