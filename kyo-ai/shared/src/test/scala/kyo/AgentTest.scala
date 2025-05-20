package kyo

import kyo.*
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description
import org.scalatest.compatible.Assertion

class AgentTest extends Test:

    case class Query(question: String)
    case class Response(answer: String)
    case class TestError(message: String)

    "basic agent operations" - {
        "creates and processes simple messages" in run {
            for
                agent <- Agent.run[Query](query =>
                    for
                        _        <- AI.systemMessage("You are a helpful assistant.")
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                )
                response <- agent.ask(Query("What is 2+2?"))
            yield assert(response.answer.nonEmpty)
        }

        "uses provided prompt" in run {
            for
                agent <- Agent.run[Query](
                    prompt = Prompt.init("You are a helpful math assistant."),
                    tools = Nil,
                    thoughts = Nil
                )(query =>
                    for
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                )
                response <- agent.ask(Query("What is 2+2?"))
            yield assert(response.answer.nonEmpty)
        }
    }

    "context management" - {
        "uses AI context appropriately" in run {
            for
                agent <- Agent.run[Query] { query =>
                    for
                        _        <- AI.systemMessage("You are a helpful assistant")
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                }
                response <- agent.ask(Query("What is your purpose?"))
            yield assert(response.answer.nonEmpty)
        }

        "manages conversation history" in run {
            for
                agent <- Agent.run[String] { message =>
                    for
                        context <- Var.get[Context]
                        systemCount = context.messages.count(_.isInstanceOf[SystemMessage])
                        userCount   = context.messages.count(_.isInstanceOf[UserMessage])
                        _        <- AI.systemMessage("Remember the counts")
                        _        <- AI.userMessage(message)
                        response <- AI.gen[String]
                    yield s"System: $systemCount, User: $userCount, Response length: ${response.length}"
                }
                response1 <- agent.ask("First message")
                response2 <- agent.ask("Second message")
                response3 <- agent.ask("Third message")
            yield
                assert(response1.startsWith("System: 0, User: 0"))
                assert(response2.startsWith("System: 1, User: 1"))
                assert(response3.startsWith("System: 2, User: 2"))
        }
    }

    "custom behaviors" - {
        "supports custom actor behavior" in run {
            case class CountedResponse(answer: String, count: Int)

            for
                agent <- Agent.runBehavior[Query] {
                    Var.run(0) {
                        Agent.receiveLoop[Query] { query =>
                            for
                                count    <- Var.update[Int](_ + 1)
                                _        <- AI.systemMessage("You are a helpful assistant")
                                _        <- AI.userMessage(query.question)
                                response <- AI.gen[String]
                                result = CountedResponse(response, count)
                            yield Loop.continue(result)
                        }
                    }
                }
                response1 <- agent.ask(Query("First question"))
                response2 <- agent.ask(Query("Second question"))
            yield
                assert(response1.answer.nonEmpty)
                assert(response1.count == 1)
                assert(response2.answer.nonEmpty)
                assert(response2.count == 2)
            end for
        }
    }

    "thought-driven processing" - {
        case class Analysis(@description("Analyze this query carefully") analysis: String)
        case class Verification(@description("Verify your answer") correct: Boolean = true)

        "uses custom thoughts" in run {

            val customThought = Thought.aggregate(
                Thought.opening[Analysis],
                Thought.closing[Verification]
            )

            for
                agent <- Agent.run[Query](
                    thoughts = Seq(customThought)
                ) { query =>
                    for
                        _        <- AI.systemMessage("You are a thoughtful assistant")
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                }
                response <- agent.ask(Query("What is the meaning of life?"))
            yield assert(response.answer.nonEmpty)
            end for
        }
    }

    "tool integration" - {
        case class CalculateInput(expression: String)
        case class CalculateOutput(result: Double)

        "uses tools properly" in run {

            val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
                val result = input.expression match
                    case "1+2" => 3.0
                    case "2*3" => 6.0
                    case _     => 0.0
                CalculateOutput(result)
            }

            for
                agent <- Agent.run[Query](
                    tools = Seq(calculatorTool)
                ) { query =>
                    for
                        _        <- AI.systemMessage("You are an assistant with a calculator tool")
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                }
                response <- agent.ask(Query("Calculate 1+2"))
            yield assert(response.answer.nonEmpty)
            end for
        }
    }

    "concurrent processing" - {
        "handles concurrent requests" in run {
            for
                results <- Queue.Unbounded.init[Response]()
                agent <- Agent.run[Query] { query =>
                    for
                        _        <- AI.systemMessage("You are a helpful assistant")
                        _        <- AI.userMessage(query.question)
                        response <- AI.gen[Response]
                    yield response
                }
                _ <- Async.foreach(1 to 5) { i =>
                    agent.ask(Query(s"Question $i")).map(results.add)
                }
                responses <- results.drain
            yield assert(responses.size == 5)
        }
    }
end AgentTest
