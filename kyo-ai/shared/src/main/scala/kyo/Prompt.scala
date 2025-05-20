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
        using Isolate.Contextual[S, AI]
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
            val prompts   = Chunk.empty
            val reminders = Chunk.empty

        val local = Local.init[Prompt[Any]](Empty)

        val mainSeparator            = "\n\n==================================================\n\n"
        val sectionSeparator         = "\n\n--------------------------------------------------\n\n"
        val toolHeaderFormat         = "\n\n================== TOOL: %s ==================\n\n"
        val reminderHeaderFormat     = "\n\n================== REMINDERS ==================\n\n"
        val toolReminderHeaderFormat = "\n\n================== TOOL REMINDER: %s ==================\n\n"

        def enrichedContext(tools: Chunk[Tool.internal.Info[?, ?, AI]])(using Frame): Context < AI =
            Var.use[Context] { context =>
                local.use { p =>
                    val merged = p.andThen(default)
                    for
                        prompts       <- merged.prompts
                        reminders     <- merged.reminders
                        toolPrompts   <- Kyo.foreach(tools)(t => t.prompt.prompts.map(prompts => (t.name, t.description, prompts)))
                        toolReminders <- Kyo.foreach(tools)(t => t.prompt.reminders.map(reminders => (t.name, t.description, reminders)))
                    yield
                        val mainPromptsString = prompts.mkString(mainSeparator)

                        val toolPromptsString =
                            toolPrompts.map { case (toolName, toolDesc, toolPrompt) =>
                                val header      = toolHeaderFormat.format(toolName)
                                val description = if toolDesc.nonEmpty then s"DESCRIPTION: $toolDesc\n\n" else ""
                                val content     = toolPrompt.mkString(sectionSeparator)
                                s"$header$description$content"
                            }.mkString("")

                        val finalPromptString =
                            if mainPromptsString.nonEmpty && toolPromptsString.nonEmpty then
                                mainPromptsString + mainSeparator + toolPromptsString
                            else
                                mainPromptsString + toolPromptsString

                        val mainRemindersString =
                            if reminders.nonEmpty then
                                reminderHeaderFormat + reminders.mkString(sectionSeparator)
                            else
                                ""

                        val toolRemindersString =
                            toolReminders.map { case (toolName, toolDesc, toolReminder) =>
                                val header      = toolReminderHeaderFormat.format(toolName)
                                val description = if toolDesc.nonEmpty then s"DESCRIPTION: $toolDesc\n\n" else ""
                                val content     = toolReminder.mkString(sectionSeparator)
                                s"$header$description$content"
                            }.mkString("")

                        val finalReminderString =
                            if mainRemindersString.nonEmpty && toolRemindersString.nonEmpty then
                                mainRemindersString + mainSeparator + toolRemindersString
                            else
                                mainRemindersString + toolRemindersString

                        Context.empty.systemMessage(finalPromptString).merge(context).systemMessage(finalReminderString)
                    end for
                }
            }

        val default =
            given Frame = Frame.internal
            Prompt.init(
                prompt =
                    p"""
                        # Operational instructions
                        
                        Focus on the prompt content so far and make sure to adhere to the following instructions:

                        You are an AI assistant that works within a compound AI system. **CRITICAL**: You must **ONLY** respond through tool calls - **NEVER** generate direct text responses.
                        
                        ## JSON FORMAT REQUIREMENTS
                        
                        - All responses must use PROPER JSON FORMAT with fields as JSON keys (not XML parameters)
                        - NEVER use XML-style tags like <parameter name="fieldName"> in your responses
                        - NEVER use escaped quotes around field names within JSON objects
                        - All fields must use the exact keys from the provided schema
                        - Field values must match the expected types (string, boolean, number, etc.)
                        
                        ## Schema Compliance
                        
                        - All fields in schemas must be included with exact names as JSON keys
                        - **CRITICAL**: Missing even a single field will cause **IMMEDIATE EXECUTION FAILURE**
                        - Field names are often full sentences - use them exactly as provided
                        - Only generate content defined in the provided schemas
                        
                        ## Execution Rules
                        
                        - You MUST ONLY use the resultTool to provide your response
                        - Any text outside of tool calls will be ignored and cause execution failures
                        
                        ## Proper JSON Structure Example
                        
                        ```json
                        {
                            "openingThoughts": "This is my detailed analysis...",
                            "I'll strictly follow the tool's json schema": true,
                            "resultValue": "My calculated result",
                            "closingThoughts": "In conclusion..."
                        }
                        ```
                        
                        ## Incorrect Format (NEVER USE)
                        
                        ```
                        {
                            "openingThoughts": "<parameter name=\\"Reflect\\">Let me analyze...",
                            "resultValue": "<parameter name=\\"The result\\">true"
                        }
                        ```
                        
                        ## Thought-Driven Response Framework
                        
                        **CRITICAL**: Any field with a sentence-style name (e.g., `Let me analyze the key factors` or `What are the main considerations`) represents a mandatory thought process you MUST complete thoroughly. Each thought field requires detailed, specific reasoning relevant to its prompt. These are not placeholders but essential components of your analysis.
                        
                        For each thought field, you must:
                        1. Focus exclusively on the specific aspect mentioned in that field
                        2. Provide detailed, substantive reasoning specific to the current task
                        3. Ensure each thought directly influences and is reflected in your final output
                        4. Avoid generic or templated responses - each thought must contain task-specific insights
                        5. Write at least 2-3 sentences for each thought field to ensure thorough coverage
                        
                        ## Sequential Thought Process
                        
                        Process thought fields in the order they appear in the schema. Each thought should build upon previous ones, creating a coherent analytical flow. Never skip or provide minimal content for any thought field.
                        
                        ## Constant Value Fields
                        
                        Some fields have expected constant values (especially Boolean fields set to true). For example:
                        - `All requirements were satisfied`: true
                        - `I have addressed all key points`: true
                        - `The solution is complete and correct`: true
                        
                        These are not merely outputs to set - they are verification requirements your response must fulfill. When you see such fields:
                        1. Treat them as verification checkpoints
                        2. Adjust your response generation to ensure these statements become true
                        3. Only mark them as true if you've genuinely met the requirement
                        4. If you cannot truthfully set the field to its expected value, you must revise your response
                        
                        ## Verification Steps Before Submission
                        
                        Before finalizing your response:
                        1. Verify ALL fields from the schema are present - missing even ONE field will cause IMMEDIATE EXECUTION FAILURE
                        2. Review each thought field to ensure it contains detailed, specific reasoning
                        3. Verify that all thought fields have significantly influenced your final output
                        4. Confirm all boolean verification fields can truthfully be marked as true
                        5. Check that your clusters and analysis align with the specific requirements in the prompt
                        6. **CRITICAL**: Ensure your response uses PROPER JSON FORMAT with no XML tags or nested parameters
                        
                        When you have determined the answer to the user's request, you must use the resultTool to return it. This is the ONLY way to provide the final result. The resultTool validates your answer against the expected schema.
                    """,
                reminder =
                    p"""
                        ## CRITICAL REMINDER
                        
                        You MUST ONLY respond using tool calls with PROPER JSON FORMAT. NEVER use XML-style tags like <parameter name="fieldName"> in your responses. 
                        
                        ### JSON FORMAT REMINDER
                        
                        - Use standard JSON format with keys and values
                        - Field names must be exact schema keys, not wrapped in additional quotes or tags
                        - Example: "fieldName": "value", NOT "<parameter name=\\"fieldName\\">value"
                        
                        ### EXECUTION FAILURE WARNING
                        
                        Missing even a SINGLE field from the schema will cause IMMEDIATE EXECUTION FAILURE. Double-check that EVERY field from the schema is included before submitting.
                        
                        ### THOUGHT FIELD REMINDER
                        
                        Every sentence-style field requires detailed, specific reasoning - not generic statements. Each thought must be thoroughly developed with at least 2-3 substantive sentences that directly address the specific aspect mentioned. Review all thought fields before submission to ensure none are skipped or minimally addressed.
                    """
            )
        end default
    end internal

end Prompt
