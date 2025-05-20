package kyo.ai.mode

import kyo.*
import kyo.AI
import kyo.Mode
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json

/** Implements a sampling technique that generates multiple responses with varying parameters and synthesizes them to improve output quality
  * and reduce hallucinations.
  *
  * The `TemperatureSampling` class creates multiple responses by varying temperature and random seed values, then has the model analyze
  * these responses for inconsistencies and synthesize a final high-quality answer. All responses are generated in parallel, minimizing
  * latency impact compared to sequential generation.
  *
  * @constructor
  *   Creates a new TemperatureSampling instance with specified parameters
  * @param generations
  *   The number of different responses to generate (typically 3-5)
  * @param maxTemperature
  *   The maximum temperature value to use for sampling (between 0-2)
  *
  * @example
  *   {{{
  *     // Generate 5 responses with temperatures up to 1.0
  *     val sampling = TemperatureSampling(5, 1.0)
  *
  *     // Apply to a generation
  *     AI.gen[String]("What is quantum computing?").handle(sampling.enable)
  *   }}}
  *
  * @note
  *   While this technique increases computation by generating multiple responses in parallel, the latency increase is much less than
  *   sequential generation. This makes it practical for scenarios where accuracy and reliability are particularly important.
  */

class TemperatureSampling(generations: Int)(using Frame) extends Mode[Any]:

    def apply[A: Json](gen: Maybe[A] < AI)(using Frame) =
        AI.forget {
            Async.fill(generations) {
                for
                    temp <- Random.nextDouble
                    seed <- Random.nextInt
                yield Config.update(_.temperature(temp).seed(Present(seed)))(gen)
            }
        }.map { alternatives =>
            AI.updateContext(
                _.systemMessage(prompt).systemMessage(
                    alternatives.flatten.map(Json[A].encode).mkString("\n\n")
                )
            ).andThen(gen)
        }

    private def prompt =
        p"""
            The following responses were generated using different random seeds and temperatures. Please:

            1) Identify specific inconsistencies between responses (different facts, figures, or claims) as these often signal hallucinations
            2) Pay particular attention to precise details that vary across responses - names, dates, numbers, quotes, and specific technical claims
            3) Create a final comprehensive answer that:
            - Builds primarily on information that appears consistently across multiple responses
            - Removes any factual claims that appear in only one response or contradict other responses
            - Explicitly acknowledges uncertainty rather than inventing details
            - Avoids overconfidence in areas where the responses show variation

            Your goal is to produce a single high-quality response that leverages collective insights while rigorously filtering out potential hallucinations and fabricated information.
        """

end TemperatureSampling
