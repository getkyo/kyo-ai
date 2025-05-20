package kyo

import kyo.ai.Context.AssistantMessage
import kyo.ai.json.Json

/** A middleware abstraction for intercepting and transforming AI generation operations.
  *
  * The `Mode` trait provides a powerful way to intercept and transform the core AI generation process without modifying client code. When
  * enabled, a Mode intercepts all `AI.gen` calls within its scope, allowing complete customization of how the generation happens.
  *
  * This interception capability enables sophisticated transformations of the generation process. A Mode can dynamically switch between
  * different models based on query complexity, customize generation parameters like temperature or token limits per request, or run
  * generations with multiple configurations in parallel. It can implement advanced prompting techniques such as few-shot learning or
  * chain-of-thought, add specialized pre- or post-processing steps, and transparently retry failed generations with modified parameters.
  *
  * For example, a Mode might intercept a simple `AI.gen[String]` call and transform it by generating responses from multiple models with
  * varying configurations, comparing the quality or consistency of these responses, synthesizing a final response from the best parts of
  * each, and returning this synthesized response to the original caller - all without the caller being aware of these enhancements.
  *
  * Modes are composable, with multiple modes able to be active simultaneously. They form a processing pipeline where each Mode transforms
  * the operation before passing it to the next Mode in the chain. This enables building complex behaviors through composition of simpler
  * transformations, creating a flexible and extensible framework for enhancing AI operations.
  *
  * @tparam S
  *   The capability set required by this mode
  */
trait Mode[-S]:

    /** Applies this mode to transform an AI generation operation.
      *
      * This method intercepts an AI generation operation and applies custom transformations before, during, or after the generation
      * process. It forms the core of the Mode's interception capability.
      *
      * @param gen
      *   The original AI generation operation to transform
      * @return
      *   A transformed AI generation operation that includes this mode's behavior
      */
    def apply[A: Json](gen: Maybe[A] < AI)(using Frame): Maybe[A] < (AI & S)

    /** Enables this mode for the duration of the provided computation.
      *
      * This method activates the mode for the scope of the given computation, causing all AI generation operations within that scope to be
      * intercepted and transformed by this mode.
      *
      * @param v
      *   The computation to run with this mode enabled
      * @return
      *   The result of the computation, with this mode applied to all AI generations
      */
    def enable[A, S2](v: A < S2)(using Frame): A < (S & S2) =
        Mode.enable(this)(v)
end Mode

object Mode:

    import internal.*

    /** Enables a specific mode for the duration of the provided computation.
      *
      * This method activates the given mode for the scope of the computation, causing all AI generation operations within that scope to be
      * intercepted and transformed by the mode.
      *
      * Example:
      * {{{
      * val result = Mode.enable(DebugMode) {
      *   AI.gen[String]("What is the capital of France?")
      * }
      * }}}
      *
      * @param mode
      *   The mode to enable
      * @param v
      *   The computation to run with the mode enabled
      * @return
      *   The result of the computation, with the mode applied to all AI generations
      */
    def enable[A, S](mode: Mode[S])(v: A < S)(using Frame): A < S =
        local.update(_.append(mode.asInstanceOf[Mode[AI]]))(v)

    private[kyo] object internal:
        val local = Local.init(Chunk.empty[Mode[AI]])

        private[kyo] def handle[A: Json](gen: Maybe[A] < AI)(using Frame): Maybe[A] < AI =
            local.use { modes =>
                def loop(modes: Chunk[Mode[AI]]): Maybe[A] < AI =
                    if modes.isEmpty then
                        gen
                    else
                        modes.head(loop(modes.tail))
                loop(modes)
            }
    end internal
end Mode
