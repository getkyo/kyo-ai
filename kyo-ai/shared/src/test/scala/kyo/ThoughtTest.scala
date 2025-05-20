package kyo

import kyo.*
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description
import scala.collection.immutable.ListMap

class ThoughtTest extends Test:

    val testConfig =
        Config.OpenAI.gpt_4o
            .temperature(0.1)
            .seed(Present(42))

    "thought aggregation" - {
        "combine multiple thoughts" in run {
            val thought1 = Thought.opening[String]
            val thought2 = Thought.closing[Boolean]
            val combined = Thought.aggregate(thought1, thought2)

            val infos = combined.infos
            assert(infos.size == 2)
            assert(infos.exists(_.position == Thought.Position.Opening))
            assert(infos.exists(_.position == Thought.Position.Closing))
        }
    }

    "thought processing" - {
        case class Analysis(@description("Let me analyze this") analysis: String)
        case class Verification(@description("Verify my answer") correct: Boolean = true)

        "processes opening thought" in run {
            for
                processed <- AtomicRef.init(false)
                analysisThought = Thought.opening[Analysis] { analysis =>
                    processed.set(true)
                }
                result <- AI.run(testConfig) {
                    analysisThought.enable {
                        for
                            _      <- AI.systemMessage("You are a helpful assistant.")
                            _      <- AI.userMessage("What is 2+2?")
                            result <- AI.gen[String]
                            flag   <- processed.get
                        yield
                            assert(flag)
                            assert(result.contains("4"))
                            result
                    }
                }
            yield assert(result.contains("4"))
        }

        "processes closing thought" in run {
            for
                processed <- AtomicRef.init(false)
                verificationThought = Thought.closing[Verification] { verification =>
                    processed.set(verification.correct)
                }
                result <- AI.run(testConfig) {
                    verificationThought.enable {
                        for
                            _      <- AI.systemMessage("You are a helpful assistant.")
                            _      <- AI.userMessage("What is 2+2?")
                            result <- AI.gen[String]
                            flag   <- processed.get
                        yield
                            assert(flag)
                            assert(result.contains("4"))
                            result
                    }
                }
            yield assert(result.contains("4"))
        }

        "processes combined thoughts" in run {
            for
                analysisProcessed     <- AtomicRef.init(false)
                verificationProcessed <- AtomicRef.init(false)

                analysisThought = Thought.opening[Analysis] { analysis =>
                    analysisProcessed.set(true)
                }

                verificationThought = Thought.closing[Verification] { verification =>
                    verificationProcessed.set(verification.correct)
                }

                combinedThought = Thought.aggregate(analysisThought, verificationThought)

                result <- AI.run(testConfig) {
                    combinedThought.enable {
                        for
                            _      <- AI.systemMessage("You are a helpful assistant.")
                            _      <- AI.userMessage("What is 2+2?")
                            result <- AI.gen[String]
                            a      <- analysisProcessed.get
                            v      <- verificationProcessed.get
                        yield
                            assert(a)
                            assert(v)
                            assert(result.contains("4"))
                            result
                    }
                }
            yield assert(result.contains("4"))
        }
    }

    "custom structured thoughts" - {
        case class CustomAnalysis(
            @description("Let me break down this problem step by step") stepByStep: String,
            @description("Here is my detailed reasoning process") reasoning: String
        )

        case class CustomVerification(
            @description("I have verified this is correct") isVerified: Boolean = true,
            @description("My confidence level (1-10)") confidence: Int
        )

        "processes complex thought structure" in run {
            for
                analysisContent        <- AtomicRef.init("")
                verificationConfidence <- AtomicRef.init(0)

                analysisThought = Thought.opening[CustomAnalysis] { analysis =>
                    analysisContent.set(analysis.stepByStep)
                }

                verificationThought = Thought.closing[CustomVerification] { verification =>
                    verificationConfidence.set(verification.confidence)
                }

                customThought = Thought.aggregate(analysisThought, verificationThought)

                result <- AI.run(testConfig) {
                    customThought.enable {
                        for
                            _          <- AI.systemMessage("You are a helpful assistant that solves math problems step by step.")
                            _          <- AI.userMessage("Calculate 15 * 7")
                            result     <- AI.gen[String]
                            content    <- analysisContent.get
                            confidence <- verificationConfidence.get
                        yield
                            assert(content.nonEmpty)
                            assert(confidence > 0)
                            assert(result.contains("105"))
                            result
                    }
                }
            yield assert(result.contains("105"))
        }
    }
end ThoughtTest
