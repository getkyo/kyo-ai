package kyo

import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.completion.Completion
import kyo.ai.json.*
import kyo.debug.Debug
import scala.annotation.nowarn
import zio.schema.DeriveSchema

/** A typed effect representing interactions with large language models (LLMs).
  *
  * The `AI` effect encapsulates the state of a conversation with an LLM and provides a composable, type-safe API for generating responses,
  * managing context, and integrating with tools that extend the LLM's capabilities.
  *
  * The effect is implemented as an opaque type that combines variable state management for the conversation context (`Var[Context]`) with
  * asynchronous capabilities (`Async`).
  *
  * A key design feature is that the `AI` instance is the computation itself, not just a wrapper or service handle. This means:
  *   - Context is automatically tracked and updated through the computation chain.
  *   - Messages added to the context are preserved across operations
  *   - The effect composition naturally maintains conversational flow
  *
  * The `AI.forget` method is particularly important as it provides a transaction-like mechanism that runs a computation with the current
  * context but discards any context changes made within that computation. This allows for exploratory or preparatory interactions with the
  * LLM that don't affect the main conversation flow.
  *
  * @see
  *   [[kyo.ai.Context]] for managing conversation history
  * @see
  *   [[kyo.Tool]] for creating tools that extend LLM capabilities
  * @see
  *   [[kyo.Thought]] for structured reasoning
  * @see
  *   [[kyo.Mode]] for alternative processing modes
  * @see
  *   [[kyo.ai.Config]] for configuring LLM parameters
  */
opaque type AI <: Var[Context] & Async = Var[Context] & Async

case class AIException(
    message: String,
    cause: Text | Throwable = ""
)(using Frame) extends KyoException(message, cause)

object AI:

    export kyo.ai.Config

    /** Runs an AI computation with an empty initial context.
      *
      * This method executes the provided computation with a fresh, empty conversation context. It's the primary entry point for starting AI
      * interactions.
      *
      * @param v
      *   The AI computation to run
      * @return
      *   The result of the computation, with AI effects translated to Async effects
      */
    def run[A, S](v: A < (AI & S))(using Frame): A < (S & Async) =
        Var.run(Context.empty)(v)

    /** Runs an AI computation with an empty initial context and modified configuration.
      *
      * This method executes the provided computation with a fresh, empty conversation context and applies the given configuration function
      * to customize the AI behavior.
      *
      * @param f
      *   A function that transforms the default configuration
      * @param v
      *   The AI computation to run
      * @return
      *   The result of the computation, with AI effects translated to Async effects
      */
    def run[A, S](f: Config => Config)(v: A < (AI & S))(using Frame): A < (S & Async) =
        Config.update(f)(Var.run(Context.empty)(v))

    /** Runs an AI computation with an empty initial context and specific configuration.
      *
      * This method executes the provided computation with a fresh, empty conversation context and the specified configuration.
      *
      * @param config
      *   The configuration to use for the AI computation
      * @param v
      *   The AI computation to run
      * @return
      *   The result of the computation, with AI effects translated to Async effects
      */
    def run[A, S](config: Config)(v: A < (AI & S))(using Frame): A < (S & Async) =
        Config.enable(config)(Var.run(Context.empty)(v))

    /** Executes an AI computation in isolation, discarding any context changes.
      *
      * This method runs the provided computation with the current context but discards any changes made to the context during execution.
      * This is useful for exploratory interactions that shouldn't affect the main conversation flow.
      *
      * @param v
      *   The AI computation to run in isolation
      * @return
      *   The result of the computation, with context changes discarded
      */
    def forget[A, S](v: A < (S & AI))(using Frame): A < (S & AI) =
        Var.isolate.discard[Context].run(v)

    /** Executes an AI computation with a fresh, empty context.
      *
      * This method runs the provided computation with a new, empty context, regardless of the current context. This is useful for starting
      * a completely new conversation.
      *
      * @param v
      *   The AI computation to run with a fresh context
      * @return
      *   The result of the computation
      */
    def fresh[A, S](v: A < (S & AI))(using Frame): A < (S & AI) =
        Var.run(Context.empty)(v)

    /** Applies a configuration transformation to an AI computation.
      *
      * This method modifies the configuration used by the AI computation according to the provided transformation function.
      *
      * @param f
      *   A function that transforms the configuration
      * @param v
      *   The AI computation to run with the modified configuration
      * @return
      *   The result of the computation with the modified configuration
      */
    def withConfig[A, S](f: Config => Config)(v: A < S)(using Frame): A < (S & AI) =
        Config.update(f)(v)

    /** Applies a specific configuration to an AI computation.
      *
      * This method sets the configuration used by the AI computation to the provided value.
      *
      * @param config
      *   The configuration to use
      * @param v
      *   The AI computation to run with the specified configuration
      * @return
      *   The result of the computation with the specified configuration
      */
    def withConfig[A, S](config: Config)(v: A < S)(using Frame): A < (S & AI) =
        Config.enable(config)(v)

    /** Generates a response of type A from a JSON-encodable input.
      *
      * This method adds the input as a user message to the context and then generates a response of the specified type.
      *
      * @param input
      *   The input to encode as a user message
      * @return
      *   A computation that will produce a response of type A
      */
    def gen[A: Json](using frame: Frame)[B: Json](input: B): A < AI =
        AI.updateContext(_.userMessage(Json[B].encodePretty(input))).andThen(gen[A])

    /** Generates a response of type A from two JSON-encodable inputs.
      *
      * This method combines the inputs into a tuple, adds them as a user message to the context, and then generates a response of the
      * specified type.
      *
      * @param input1
      *   The first input
      * @param input2
      *   The second input
      * @return
      *   A computation that will produce a response of type A
      */
    def gen[A: Json](using Frame)[B: Json, C: Json](input1: B, input2: C): A < AI =
        gen[A]((input1, input2))

    /** Generates a response of type A from three JSON-encodable inputs.
      *
      * This method combines the inputs into a tuple, adds them as a user message to the context, and then generates a response of the
      * specified type.
      *
      * @param input1
      *   The first input
      * @param input2
      *   The second input
      * @param input3
      *   The third input
      * @return
      *   A computation that will produce a response of type A
      */
    def gen[A: Json](using Frame)[B: Json, C: Json, D: Json](input1: B, input2: C, input3: D): A < AI =
        gen[A]((input1, input2, input3))

    /** Generates a response of type A from four JSON-encodable inputs.
      *
      * This method combines the inputs into a tuple, adds them as a user message to the context, and then generates a response of the
      * specified type.
      *
      * @param input1
      *   The first input
      * @param input2
      *   The second input
      * @param input3
      *   The third input
      * @param input4
      *   The fourth input
      * @return
      *   A computation that will produce a response of type A
      */
    def gen[A: Json](using Frame)[B: Json, C: Json, D: Json, E: Json](input1: B, input2: C, input3: D, input4: E): A < AI =
        gen[A]((input1, input2, input3, input4))

    /** Generates a response of type A from the current context.
      *
      * This method evaluates the current context and generates a response of the specified type. It handles mode-specific processing and
      * retries within configured limits.
      *
      * @return
      *   A computation that will produce a response of type A
      */
    def gen[A: Json](using Frame): A < AI =
        Config.use { config =>
            Loop.indexed { iterations =>
                Mode.internal.handle(eval[A]).map {
                    case Present(r) =>
                        Loop.done(r)
                    case Absent =>
                        if iterations >= config.maxIterations then
                            AI.fail("Eval loop exceeded max iterations")
                        else
                            Loop.continue
                }
            }
        }

    /** Adds a system message to the conversation context.
      *
      * System messages provide instructions or context to the AI model about how it should behave. They're typically used to set the tone,
      * role, or constraints for the conversation.
      *
      * @param content
      *   The content of the system message
      * @return
      *   A computation that updates the context with the system message
      */
    def systemMessage(content: String)(using Frame): Unit < AI =
        updateContext(_.systemMessage(content))

    /** Adds a user message to the conversation context.
      *
      * User messages represent input from the user in the conversation.
      *
      * @param content
      *   The content of the user message
      * @return
      *   A computation that updates the context with the user message
      */
    def userMessage(content: String)(using Frame): Unit < AI =
        updateContext(_.userMessage(content, Absent))

    /** Adds a user message with an image to the conversation context.
      *
      * This method adds a user message that includes both text and an image.
      *
      * @param content
      *   The text content of the user message
      * @param image
      *   The image to include with the message
      * @return
      *   A computation that updates the context with the user message and image
      */
    def userMessage(content: String, image: Image)(using Frame): Unit < AI =
        updateContext(_.userMessage(content, Present(image)))

    /** Adds an assistant message to the conversation context.
      *
      * Assistant messages represent responses from the AI in the conversation.
      *
      * @param content
      *   The content of the assistant message
      * @return
      *   A computation that updates the context with the assistant message
      */
    def assistantMessage(content: String)(using Frame): Unit < AI =
        updateContext(_.assistantMessage(content))

    /** Updates the conversation context using a transformation function.
      *
      * This method applies the provided function to transform the current context.
      *
      * @param f
      *   A function that transforms the context
      * @return
      *   A computation that applies the transformation to the context
      */
    def updateContext[S](f: Context => Context)(using Frame): Unit < AI =
        Var.updateDiscard[Context](f)

    given isolate: Isolate.Stateful[AI, Async] =
        Var.isolate.merge[Context](_.merge(_)).use {
            Isolate.Stateful.derive[Var[Context] & Async, Async]
        }

    private[kyo] def fail(message: String)(using Frame): Nothing < AI =
        Abort.panic(AIException(message))

    private def eval[A: Json](using Frame): Maybe[A] < AI =
        for
            config   <- Config.get
            thoughts <- Thought.internal.infos
            tools    <- Tool.internal.infos
            json = Thought.internal.resultJson[A](thoughts)
            (resultTool, getResult) <- Tool.internal.resultTool(using json)
            allTools = tools.concat(resultTool.infos)
            context <- Prompt.internal.enrichedContext(allTools)
            message <-
                config.provider
                    .completion(config, context, allTools)
                    .handle(
                        config.meter.run,
                        Retry[Throwable](_),
                        Abort.recover[Throwable](Abort.panic)
                    )
            _ <- AI.updateContext(_.add(message))
            _ <- Tool.internal.handle(allTools, message.calls)
            r <- getResult
            _ <- r.map(r => Thought.internal.handle(thoughts, r)).getOrElse(Kyo.unit)
        yield r.map(_.resultValue)
        end for
    end eval

end AI
