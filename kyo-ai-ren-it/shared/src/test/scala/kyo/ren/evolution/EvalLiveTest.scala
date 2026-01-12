// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.network.*

// class EvalLiveTest extends Test:

//     def createSimpleMind(name: String): Mind.State =
//         val neuronState = Neuron.State(
//             name = s"${name}_processor",
//             description = "Processes input and generates response",
//             previous = List.empty,
//             next = List.empty,
//             conscious = true,
//             memory = None,
//             output = Gate.State(
//                 p"""Process the input and generate a clear, concise response.""",
//                 p"""Generate response.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             ),
//             route = None
//         )

//         Mind.State(
//             name = name,
//             maxRecursions = 3,
//             neurons = List(neuronState),
//             output = Gate.State(
//                 p"""Based on the input and thoughts, generate a response string.""",
//                 p"""Generate response.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )
//     end createSimpleMind

//     "judge correctly scores exact match" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is 2 + 2?",
//             expected = "4"
//         )

//         val perfectMindState = Mind.State(
//             name = "perfect-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "4" - nothing more.""",
//                 p"""Return 4.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(perfectMindState)
//                 score    <- eval(mindInst, testCase)
//             yield
//                 assert(
//                     score.accuracy >= 0.8,
//                     s"Judge should give high score (>=0.8) for exact match but got ${score.accuracy}"
//                 )
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//         }
//     }

//     "judge correctly scores completely wrong answer" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is 2 + 2?",
//             expected = "4"
//         )

//         val wrongMindState = Mind.State(
//             name = "wrong-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "banana" - nothing more.""",
//                 p"""Return banana.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(wrongMindState)
//                 score    <- eval(mindInst, testCase)
//             yield
//                 assert(
//                     score.accuracy <= 0.3,
//                     s"Judge should give low score (<=0.3) for completely wrong answer but got ${score.accuracy}"
//                 )
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//         }
//     }

//     "judge distinguishes between correct and incorrect answers" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is the capital of France?",
//             expected = "Paris"
//         )

//         val correctMindState = Mind.State(
//             name = "correct-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "Paris" - nothing more.""",
//                 p"""Return Paris.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         val incorrectMindState = Mind.State(
//             name = "incorrect-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "London" - nothing more.""",
//                 p"""Return London.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval           <- Eval.init(evalState)
//                 correctMind    <- Mind.init(correctMindState)
//                 incorrectMind  <- Mind.init(incorrectMindState)
//                 correctScore   <- eval(correctMind, testCase)
//                 incorrectScore <- eval(incorrectMind, testCase)
//             yield
//                 assert(
//                     correctScore.accuracy > incorrectScore.accuracy,
//                     s"Judge should score correct answer (${correctScore.accuracy}) higher than incorrect answer (${incorrectScore.accuracy})"
//                 )
//                 assert(
//                     correctScore.accuracy >= 0.7,
//                     s"Correct answer should score >=0.7 but got ${correctScore.accuracy}"
//                 )
//                 assert(
//                     incorrectScore.accuracy <= 0.4,
//                     s"Incorrect answer should score <=0.4 but got ${incorrectScore.accuracy}"
//                 )
//         }
//     }

//     "judge handles semantically equivalent answers" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What color is the sky on a clear day?",
//             expected = "blue"
//         )

//         val semanticMindState = Mind.State(
//             name = "semantic-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "The sky is blue" - nothing more.""",
//                 p"""Return sky is blue.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(semanticMindState)
//                 score    <- eval(mindInst, testCase)
//             yield assert(
//                 score.accuracy >= 0.6,
//                 s"Judge should give decent score (>=0.6) for semantically equivalent answer but got ${score.accuracy}"
//             )
//         }
//     }

//     "judge handles partially correct answers" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is 10 + 10?",
//             expected = "20"
//         )

//         val partialMindState = Mind.State(
//             name = "partial-answer",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "The answer is 20" - nothing more.""",
//                 p"""Return answer is 20.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(partialMindState)
//                 score    <- eval(mindInst, testCase)
//             yield assert(
//                 score.accuracy >= 0.5,
//                 s"Judge should give decent score (>=0.5) for answer containing correct value but got ${score.accuracy}"
//             )
//         }
//     }

//     "judge scores numeric accuracy" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is 7 times 8?",
//             expected = "56"
//         )

//         val exactMindState = Mind.State(
//             name = "exact-number",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "56" - nothing more.""",
//                 p"""Return 56.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         val wrongMindState = Mind.State(
//             name = "wrong-number",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "banana" - nothing more.""",
//                 p"""Return banana.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval       <- Eval.init(evalState)
//                 exactMind  <- Mind.init(exactMindState)
//                 wrongMind  <- Mind.init(wrongMindState)
//                 exactScore <- eval(exactMind, testCase)
//                 wrongScore <- eval(wrongMind, testCase)
//             yield
//                 assert(
//                     exactScore.accuracy >= 0.8,
//                     s"Exact answer should score >=0.8 but got ${exactScore.accuracy}"
//                 )
//                 assert(
//                     exactScore.accuracy > wrongScore.accuracy,
//                     s"Exact answer (${exactScore.accuracy}) should score higher than completely wrong answer (${wrongScore.accuracy})"
//                 )
//         }
//     }

//     "judge evaluates complex explanations" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is a variable in programming?",
//             expected = "A named storage location that holds a value"
//         )

//         val goodExplanationState = Mind.State(
//             name = "good-explanation",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "A variable is a named place to store data" - nothing more.""",
//                 p"""Return variable explanation.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         val poorExplanationState = Mind.State(
//             name = "poor-explanation",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Return exactly: "Something in code" - nothing more.""",
//                 p"""Return vague explanation.""",
//                 Temperature.precise,
//                 History.State(List.empty, 0)
//             )
//         )

//         AI.run {
//             for
//                 eval      <- Eval.init(evalState)
//                 goodMind  <- Mind.init(goodExplanationState)
//                 poorMind  <- Mind.init(poorExplanationState)
//                 goodScore <- eval(goodMind, testCase)
//                 poorScore <- eval(poorMind, testCase)
//             yield
//                 assert(
//                     goodScore.accuracy > poorScore.accuracy,
//                     s"Good explanation (${goodScore.accuracy}) should score higher than poor explanation (${poorScore.accuracy})"
//                 )
//                 assert(
//                     goodScore.accuracy >= 0.5,
//                     s"Good explanation should score >=0.5 but got ${goodScore.accuracy}"
//                 )
//         }
//     }

//     "measures latency for evaluation" in run {
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is the capital of France?",
//             expected = "Paris"
//         )

//         AI.run {
//             for
//                 eval      <- Eval.init(evalState)
//                 mindInst  <- Mind.init(createSimpleMind("geography"))
//                 startTime <- Clock.now
//                 score     <- eval(mindInst, testCase)
//                 endTime   <- Clock.now
//                 duration = endTime - startTime
//             yield
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//                 assert(
//                     score.latency.toMillis <= duration.toMillis,
//                     s"Measured latency ${score.latency} should be less than total duration ${duration}"
//                 )
//         }
//     }

//     "evaluates mind with network" in run {
//         val analyzerState = Neuron.State(
//             name = "analyzer",
//             description = "Analyzes input",
//             previous = List.empty,
//             next = List("responder"),
//             conscious = false,
//             memory = None,
//             output = Gate.State(
//                 p"""Analyze the input question.""",
//                 p"""Analyze question.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             ),
//             route = None
//         )

//         val responderState = Neuron.State(
//             name = "responder",
//             description = "Generates response",
//             previous = List("analyzer"),
//             next = List.empty,
//             conscious = true,
//             memory = None,
//             output = Gate.State(
//                 p"""Based on the analysis, generate a clear response.""",
//                 p"""Generate response.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             ),
//             route = None
//         )

//         val mindState = Mind.State(
//             name = "network-mind",
//             maxRecursions = 5,
//             neurons = List(analyzerState, responderState),
//             output = Gate.State(
//                 p"""Based on the input and network thoughts, generate a response string.""",
//                 p"""Generate response.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )

//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is machine learning?",
//             expected = "A type of artificial intelligence that enables systems to learn from data"
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(mindState)
//                 score    <- eval(mindInst, testCase)
//             yield
//                 assert(score.accuracy >= 0.0 && score.accuracy <= 1.0, s"Accuracy must be between 0 and 1: ${score.accuracy}")
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//         }
//     }

//     "evaluates mind with memory-enhanced neuron" in run {
//         val neuronState = Neuron.State(
//             name = "knowledge-processor",
//             description = "Processes input using knowledge base",
//             previous = List.empty,
//             next = List.empty,
//             conscious = true,
//             memory = Some(Memory.State(
//                 List(
//                     Memory.Entry(0, 0, "Python is a high-level programming language"),
//                     Memory.Entry(0, 0, "Python was created by Guido van Rossum")
//                 ),
//                 Gate.State(
//                     p"""Retrieve relevant memory entries for answering the question.""",
//                     p"""Retrieve memories.""",
//                     Temperature.balanced,
//                     History.State(List.empty, 0)
//                 )
//             )),
//             output = Gate.State(
//                 p"""Using the memory context, answer the question accurately.""",
//                 p"""Answer with memory.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             ),
//             route = None
//         )

//         val mindState = Mind.State(
//             name = "memory-mind",
//             maxRecursions = 3,
//             neurons = List(neuronState),
//             output = Gate.State(
//                 p"""Based on the input and thoughts, generate a response string.""",
//                 p"""Generate response.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )

//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What type of language is Python?",
//             expected = "high-level programming language"
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(mindState)
//                 score    <- eval(mindInst, testCase)
//             yield
//                 assert(score.accuracy >= 0.0 && score.accuracy <= 1.0, s"Accuracy must be between 0 and 1: ${score.accuracy}")
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//         }
//     }

//     "evaluates multiple test cases" in run {
//         val mindState = createSimpleMind("multi-evaluator")
//         val evalState = Eval.State(customJudge = None)

//         val testCases = List(
//             Evolution.TestCase("0", "What is 5 + 5?", "10"),
//             Evolution.TestCase("0", "What is the opposite of hot?", "cold"),
//             Evolution.TestCase("0", "Name a primary color", "red")
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(mindState)
//                 scores   <- Kyo.foreach(testCases)(tc => eval(mindInst, tc))
//             yield
//                 assert(scores.size == 3, s"Should have 3 scores but got ${scores.size}")
//                 scores.foreach { score =>
//                     assert(score.accuracy >= 0.0 && score.accuracy <= 1.0, s"Each accuracy must be between 0 and 1: ${score.accuracy}")
//                     assert(score.latency.toMillis > 0, s"Each latency should be positive: ${score.latency}")
//                 }
//                 succeed
//         }
//     }

//     "handles incorrect answers with low scores" in run {
//         val mindState = createSimpleMind("faulty-mind")
//         val evalState = Eval.State(customJudge = None)
//         val testCase = Evolution.TestCase(
//             id = 0,
//             input = "What is 100 + 100?",
//             expected = "200"
//         )

//         AI.run {
//             for
//                 eval     <- Eval.init(evalState)
//                 mindInst <- Mind.init(mindState)
//                 score    <- eval(mindInst, testCase)
//             yield
//                 assert(score.accuracy >= 0.0 && score.accuracy <= 1.0, s"Accuracy must be between 0 and 1: ${score.accuracy}")
//                 assert(score.latency.toMillis > 0, s"Latency should be positive: ${score.latency}")
//         }
//     }

//     "compares latencies across evaluations" in run {
//         val mindState = createSimpleMind("speed-tester")
//         val evalState = Eval.State(customJudge = None)

//         val simpleCase = Evolution.TestCase(
//             id = 0,
//             input = "Yes or no?",
//             expected = "yes"
//         )

//         val complexCase = Evolution.TestCase(
//             id = 0,
//             input = "Explain the theory of relativity in simple terms",
//             expected = "Space and time are interconnected, and gravity affects both"
//         )

//         AI.run {
//             for
//                 eval         <- Eval.init(evalState)
//                 mindInst     <- Mind.init(mindState)
//                 simpleScore  <- eval(mindInst, simpleCase)
//                 complexScore <- eval(mindInst, complexCase)
//             yield
//                 assert(simpleScore.latency.toMillis > 0, s"Simple latency should be positive: ${simpleScore.latency}")
//                 assert(complexScore.latency.toMillis > 0, s"Complex latency should be positive: ${complexScore.latency}")
//                 assert(
//                     simpleScore.accuracy >= 0.0 && simpleScore.accuracy <= 1.0,
//                     s"Simple accuracy must be between 0 and 1: ${simpleScore.accuracy}"
//                 )
//                 assert(
//                     complexScore.accuracy >= 0.0 && complexScore.accuracy <= 1.0,
//                     s"Complex accuracy must be between 0 and 1: ${complexScore.accuracy}"
//                 )
//         }
//     }

// end EvalLiveTest
