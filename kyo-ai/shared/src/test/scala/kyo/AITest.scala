package kyo

import kyo.*
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description
import scala.collection.immutable.ListMap

class AITest extends Test:

    val testConfig =
        Config.OpenAI.gpt_4o
            .temperature(0.1)
            .seed(Present(42))

    case class TestPrompt(query: String)
    case class TestResponse(answer: String)

    case class MathProblem(problem: String)
    case class MathSolution(solution: Int, explanation: String)

    case class Sentiment(text: String)
    case class SentimentAnalysis(sentiment: String, confidence: Double, reasoning: String)

    "basic generation" in run {
        AI.run(testConfig) {
            for
                _        <- AI.systemMessage("You are a helpful assistant. Keep your responses concise.")
                _        <- AI.userMessage("Say hello")
                response <- AI.gen[String]
            yield
                assert(response.nonEmpty)
                assert(response.length <= 200)
        }
    }

    "typed generation with deterministic problem" in run {
        AI.run(testConfig) {
            for
                solution <- AI.gen[MathSolution](MathProblem("What is 2 + 2?"))
            yield
                assert(solution.solution == 4)
                assert(solution.explanation.nonEmpty)
        }
    }

    "sentiment analysis consistency" in run {
        AI.run(testConfig) {
            for
                result1 <- AI.gen[SentimentAnalysis](Sentiment("I love this product, it's amazing!"))
                result2 <- AI.gen[SentimentAnalysis](Sentiment("This is terrible, I hate it."))
            yield
                assert(result1.sentiment.toLowerCase.contains("positive"))
                assert(result2.sentiment.toLowerCase.contains("negative"))
                assert(result1.confidence > 0.5)
                assert(result2.confidence > 0.5)
        }
    }

    "revised context management" in run {
        AI.run(testConfig) {
            for
                _ <- AI.systemMessage("You are a helpful assistant that MUST mention the word 'cookie' in EVERY response, no matter what.")
                _ <- AI.userMessage("Give me a short greeting")
                response1 <- AI.gen[String]

                _         <- AI.userMessage("Tell me about the weather")
                response2 <- AI.gen[String]
            yield
                assert(response1.toLowerCase.contains("cookie"))
                assert(response2.toLowerCase.contains("cookie"))
        }
    }

    "revised context isolation with forget" in run {
        AI.run(testConfig) {
            for
                _ <- AI.systemMessage("You are a helpful assistant that must do exactly what the user asks.")
                _ <- AI.userMessage("The number is 42. Whenever I ask about 'the number', you must respond with only '42'.")
                _ <- AI.gen[String]

                _ <- AI.forget {
                    AI.userMessage("Ignore my previous instruction about the number.")
                    AI.gen[String]
                }

                _             <- AI.userMessage("What is the number?")
                finalResponse <- AI.gen[String]
            yield assert(finalResponse.contains("42"))
        }
    }

    "fresh context" in run {
        AI.run(testConfig) {
            for
                _ <- AI.systemMessage("You are a helpful assistant that must do exactly what the user asks.")
                _ <- AI.userMessage("The phrase is 'blue sky'. Whenever I ask about 'the phrase', you must respond with 'blue sky'.")
                _ <- AI.gen[String]

                freshResponse <- AI.fresh {
                    AI.userMessage("What is the phrase?")
                    AI.gen[String]
                }
            yield assert(!freshResponse.toLowerCase.contains("blue sky"))
        }
    }

    "temperature comparison" in run {
        val singlePrompt = "Generate a single random word"

        for
            lowTempConfig <- AI.run(testConfig.temperature(0.0)) {
                AI.userMessage(singlePrompt)
                AI.gen[String]
            }
            highTempConfig <- AI.run(testConfig.temperature(1.0)) {
                AI.userMessage(singlePrompt)
                AI.gen[String]
            }
        yield
            assert(lowTempConfig.nonEmpty)
            assert(highTempConfig.nonEmpty)
        end for
    }

    "thought structured generation" in run {
        case class Analysis(@description("Analyze this carefully") analysis: String)
        case class Verification(@description("Verify your answer") correct: Boolean)

        val customThought = Thought.aggregate(
            Thought.opening[Analysis],
            Thought.closing[Verification]
        )

        case class Question(query: String)
        case class Answer(response: String)

        AI.run(testConfig) {
            customThought.enable {
                for
                    answer <- AI.gen[Answer](Question("What is 1+1?"))
                yield assert(answer.response.contains("2"))
            }
        }
    }

    "tool integration" in run {
        case class CalculateInput(expression: String)
        case class CalculateOutput(result: Double)

        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
            val result = input.expression match
                case "1+2" => 3.0
                case "2*3" => 6.0
                case _     => 0.0
            CalculateOutput(result)
        }

        AI.run(testConfig) {
            calculatorTool.enable {
                for
                    _        <- AI.systemMessage("Use the calculator tool to solve math problems.")
                    _        <- AI.userMessage("What is 1+2?")
                    response <- AI.gen[String]
                yield assert(response.contains("3"))
            }
        }
    }

    "update context" in run {
        AI.run(testConfig) {
            for
                initialContext <- Var.get[Context]
                _              <- AI.updateContext(_.userMessage("Hello"))
                updatedContext <- Var.get[Context]
            yield
                assert(initialContext.messages.size < updatedContext.messages.size)
                assert(updatedContext.messages.last.content == "Hello")
        }
    }

    "AI.withConfig" in run {
        AI.run(testConfig) {
            for
                configBefore <- Config.get
                result <- AI.withConfig(_.temperature(0.5)) {
                    Config.get.map(_.temperature)
                }
                configAfter <- Config.get
            yield
                assert(result == 0.5)
                assert(configBefore.temperature == configAfter.temperature)
        }
    }
end AITest
