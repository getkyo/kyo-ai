package kyo

import kyo.AI
import kyo.ai.Context
import scala.annotation.nowarn
import scala.language.implicitConversions

/** A composable prompt system with floating reminders to maintain instruction adherence.
  *
  * The `Prompt` trait in Kyo represents a sophisticated approach to prompt engineering that addresses a fundamental challenge in LLM
  * interactions: instruction forgetting in long contexts.
  *
  * Unlike traditional prompt systems that place all instructions at the beginning of the context, Kyo's design splits guidance into two
  * distinct components:
  *
  *   - Primary instructions: Detailed guidance placed at the start of the context
  *   - Reminders: Brief, focused summaries of critical instructions that "float" at the end of the context
  *
  * This dual structure is crucial for reliable LLM behavior. As conversations grow longer, models often forget initial instructions since
  * they're pushed far back in the context window. Reminders solve this problem by always appearing immediately before the generation point.
  * They should be concise distillations of the most important aspects of the initial instructions - not new instructions, but
  * reinforcements of existing ones.
  *
  * For example, if the primary instruction contains detailed guidance on code formatting, the reminder might simply state: "Remember to
  * format all code with proper syntax highlighting." This brevity ensures reminders don't consume excessive context space while still
  * maintaining the model's adherence to critical guidelines.
  *
  * The system integrates seamlessly with tools, automatically including tool-specific instructions and reminders when tools are enabled.
  * This design significantly improves reliability in complex applications where maintaining consistent model behavior throughout extended
  * interactions is essential.
  *
  * To optimize prompt caching, the system adds instructions as separate system messages rather than concatenating them. This allows LLM
  * providers to cache individual instruction blocks and reuse them across different prompt combinations, significantly improving
  * performance for applications with shared instruction sets.
  *
  * The library also provides a convenient `p` string interpolator for creating well-formatted prompts by automatically trimming whitespace
  * and normalizing line breaks.
  *
  * @tparam S
  *   The capability set required by this prompt
  */
sealed trait Prompt[-S]:

    /** Enables this prompt for a computation with additional capabilities.
      *
      * @param v
      *   The computation to run with the enabled prompt
      * @return
      *   The computation with the prompt enabled and combined capabilities
      */
    final def enable[A, S2](v: A < S2)(using Frame): A < (S & S2) =
        Prompt.enable(this)(v)

    /** Combines this prompt with another prompt.
      *
      * This method merges the primary instructions and reminders of both prompts. If both prompts are identical, it returns this prompt.
      *
      * @param other
      *   The prompt to combine with this one
      * @return
      *   A new prompt containing the combined instructions and reminders
      */
    final def andThen[S2](other: Prompt[S2])(using Frame): Prompt[S & S2] =
        if this.equals(other) then this
        else
            Prompt._init[S & S2](
                for
                    p0 <- prompts
                    p1 <- other.prompts
                yield p0.concat(p1).distinct,
                for
                    r0 <- reminders
                    r1 <- other.reminders
                yield r0.concat(r1).distinct
            )

    private[kyo] val prompts: Chunk[String] < (AI & S)
    private[kyo] val reminders: Chunk[String] < (AI & S)

end Prompt

extension (sc: StringContext)
    /** A string interpolator for creating well-formatted prompts.
      *
      * This interpolator automatically normalizes whitespace by:
      *   - Replacing sequences of newlines followed by spaces with a single newline
      *   - Trimming leading and trailing whitespace
      *
      * This makes it easier to write multi-line prompts in code while maintaining clean formatting in the final output.
      *
      * @param args
      *   The arguments to interpolate into the string
      * @return
      *   A formatted prompt string with normalized whitespace
      */
    def p(args: Any*): String =
        sc.s(args*)
            .replaceAll("\n\\s+", "\n")
            .trim
end extension

object Prompt:

    import internal.*

    /** Creates an empty prompt with no instructions or reminders.
      *
      * @return
      *   An empty Prompt instance
      */
    def empty: Prompt[Any] = Empty

    /** Enables a prompt for a computation.
      *
      * This method applies the provided prompt to the computation, making it available in the context when the computation is executed.
      *
      * @param prompt
      *   The prompt to enable
      * @param v
      *   The computation to run with the enabled prompt
      * @return
      *   The computation with the prompt enabled
      */
    def enable[A, S](prompt: Prompt[S])(v: A < S)(using Frame): A < S =
        local.update(_.andThen(prompt.asInstanceOf[Prompt[Any]]))(v)

    private[kyo] def fresh[A, S](prompt: Prompt[S])(v: A < S)(using Frame): A < S =
        local.update(_ => prompt.asInstanceOf[Prompt[Any]])(v)

    /** Creates a new prompt with the specified primary instructions and optional reminders.
      *
      * This method creates a prompt with the given primary instructions that will be placed at the beginning of the context, and optional
      * reminders that will "float" at the end of the context.
      *
      * @param prompt
      *   The primary instructions for the prompt
      * @param reminder
      *   Optional reminders that will appear at the end of the context
      * @return
      *   A new Prompt instance
      */
    inline def init[S](
        using Isolate[S, AI, S]
    )(
        inline prompt: => String < (AI & S),
        inline reminder: => String < (AI & S) = ""
    )(using Frame): Prompt[S] =
        _init(prompt.map(Chunk(_)), reminder.map(Chunk(_)))

    @nowarn("msg=anonymous")
    private inline def _init[S](inline _prompts: => Chunk[String] < (AI & S), inline _reminders: => Chunk[String] < (AI & S))(using
        Frame
    ): Prompt[S] =
        new Prompt:
            val prompts   = _prompts
            val reminders = _reminders

    private[kyo] object internal:

        case object Empty extends Prompt[Any]:
            val prompts = Chunk.empty
            val reminders = Chunk(p"""
                1. DO NOT include any newlines after your response
                2. End your response immediately after the closing brace/tag
                3. Your response must contain ONLY the JSON
                4. The JSON must strictly follow the schema, including in its semantics
                5. DO NOT OUTPUT A LARGE SEQUENCE OF NEW LINE OR RETURN CHARACTERS.
                6. DO NOT output regular text replies, especially empty. 
                7. Always perform at **least one tool call**, it's the only agency you have.
                8. Producing completions without tool calls will result in **complete failure**
                9. DO NOT use a `json` block, do not output regular text. !!!Follow the proper tool call format!!!!

                Example tool call:

                {"choices":[{"message":{"role":"assistant","content":"","tool_calls":[{"id":"chatcmpl-tool-b1aaa715285a418e830a50ecb8ac8efe","function":{"arguments":"{\"reasoning\":\"Analyzing the given training examples to identify a pattern or rule that can be applied to the test case.\",\"nonEmptyRequiredGridIds\":[\"g@022bdc\"],\"nonEmptyRequiredOperations\":[{\"AnalysisParams\":{\"connectivity\":{\"EightConnected\":{}},\"backgroundColor\":0,\"minPixels\":1}}]}","name":"grid_tool"},"type":"function"}],"tool_call_id":null}}]}
            """)
        end Empty

        val local = Local.init[Prompt[Any]](Empty)

        def enrichedContext(tools: Chunk[Tool.internal.Info[?, ?, AI]])(using Frame): Context < AI =
            Var.use[Context] { context =>
                local.use { p =>
                    for
                        prompts       <- p.prompts
                        reminders     <- p.reminders
                        toolPrompts   <- Kyo.foreach(tools)(t => t.prompt.prompts.map(prompts => (t.name, t.description, prompts)))
                        toolReminders <- Kyo.foreach(tools)(t => t.prompt.reminders.map(reminders => (t.name, t.description, reminders)))
                    yield
                        // Build prompt messages (to be added before merging with context)
                        val mainPromptMessages = prompts

                        val toolPromptMessages = toolPrompts.flatMap { case (toolName, toolDesc, toolPromptChunk) =>
                            val header      = s"================== TOOL: $toolName ==================\n\n"
                            val description = if toolDesc.nonEmpty then s"DESCRIPTION: $toolDesc\n\n" else ""
                            toolPromptChunk.map(prompt => s"$header$description$prompt")
                        }

                        // Build reminder messages (to be added after merging with context)
                        val mainReminderMessages =
                            if reminders.nonEmpty then
                                val reminderHeader  = "================== REMINDERS ==================\n\n"
                                val reminderContent = reminders.mkString("\n\n--------------------------------------------------\n\n")
                                Chunk(s"$reminderHeader$reminderContent")
                            else
                                Chunk.empty

                        val toolReminderMessages = toolReminders.flatMap { case (toolName, toolDesc, toolReminderChunk) =>
                            if toolReminderChunk.nonEmpty then
                                val header      = s"================== TOOL REMINDER: $toolName ==================\n\n"
                                val description = if toolDesc.nonEmpty then s"DESCRIPTION: $toolDesc\n\n" else ""
                                val content     = toolReminderChunk.mkString("\n\n--------------------------------------------------\n\n")
                                Chunk(s"$header$description$content")
                            else
                                Chunk.empty
                        }

                        // Apply prompts first, then merge with context, then apply reminders
                        val allPromptMessages   = mainPromptMessages ++ toolPromptMessages
                        val allReminderMessages = mainReminderMessages ++ toolReminderMessages

                        val contextWithPrompts = allPromptMessages.foldLeft(Context.empty)((ctx, msg) => ctx.systemMessage(msg))
                        val mergedContext      = contextWithPrompts.merge(context)
                        val finalContext       = allReminderMessages.foldLeft(mergedContext)((ctx, msg) => ctx.systemMessage(msg))

                        finalContext
                    end for
                }
            }

    end internal

end Prompt
