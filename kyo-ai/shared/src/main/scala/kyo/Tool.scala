package kyo

import kyo.AI
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description
import scala.annotation.nowarn

/** A type-safe, automatically managed function with integrated prompting that the language model can invoke.
  *
  * The `Tool` trait in Kyo represents a callable function available to the language model during generation. Unlike typical LLM libraries
  * that require manual schema definition, function registration, and call handling, Kyo's Tool framework automates the entire process.
  *
  * A distinctive feature of Kyo's tool system is integrated prompting. Each tool can define its own specialized instructions and reminders
  * that are automatically included in the context when the tool is enabled:
  *
  * {{{
  * Tool.init[City](
  *   "getWeather",
  *   "Get weather for a city",
  *   Prompt.init(
  *     "For the weather tool, ensure you format city names correctly.",
  *     "Verify city names are valid before calling the weather tool."
  *   )
  * )(city => WeatherAPI.fetch(city.name))
  * }}}
  *
  * This integrated prompting ensures the model receives precise guidance on when and how to use each tool. Tool prompts are properly
  * namespaced within the context, appearing in dedicated sections that clearly associate them with specific functions.
  *
  * The tool system handles all technical aspects automatically:
  *   - Tool definitions are derived from Scala types with automatic JSON schema generation
  *   - Tool registration happens implicitly when tools are enabled for a computation
  *   - Tool calls are automatically detected, parsed, and dispatched to the appropriate function
  *   - Results are automatically serialized and returned to the model
  *   - Error handling is built-in, with failed tool calls properly communicated back to the model
  *
  * Tools can be composed and shared across modules, enabling the creation of domain-specific toolsets that extend the language model's
  * capabilities in a structured, type-safe way.
  *
  * @tparam S
  *   The capability set required by this tool
  */
sealed trait Tool[-S]:

    def enable[A, S2](v: A < S2)(using Frame): A < (S & S2 & AI) =
        Tool.enable(this)(v)

    private[kyo] def infos: Seq[Tool.internal.Info[?, ?, S]]
end Tool

object Tool:

    import internal.*

    /** Creates and initializes a new tool with the specified name, description, and prompt.
      *
      * This is the primary way to create tools in Kyo. It provides a type-safe interface for defining a tool's behavior through an input
      * type, output type, and implementation function.
      *
      * Example:
      * {{{
      * val weatherTool = Tool.init[City](
      *   "getWeather",
      *   "Get weather for a city",
      *   Prompt.init("For the weather tool, ensure you format city names correctly.")
      * )(city => WeatherAPI.fetch(city.name))
      * }}}
      *
      * @param name
      *   The name of the tool, which will be used by the language model to invoke it
      * @param description
      *   A description of what the tool does, helping the model understand when to use it
      * @param prompt
      *   Optional specialized instructions for the model about how to use this tool
      * @param run
      *   The function that implements the tool's behavior, taking an input of type `In` and producing an output of type `Out`
      * @tparam In
      *   The input type that the tool accepts, must have a Json instance
      * @tparam Out
      *   The output type that the tool produces, must have a Json instance
      * @tparam S
      *   The capability set required by this tool
      * @return
      *   A new tool instance
      */
    def init[In](using Json[In])[Out: Json, S](
        using Isolate.Contextual[S, AI]
    )(
        name: String,
        description: String = "",
        prompt: Prompt[S] = Prompt.empty
    )(
        run: In => Out < (AI & S)
    )(using frame: Frame): Tool[S] =
        new Tool[S]:
            def infos = Seq(Info(name, description, prompt, run, Json[In], Json[Out], frame))

    /** Enables a set of tools for a computation.
      *
      * This method makes the specified tools available to the language model during the execution of the given computation. Tools are
      * automatically registered with the AI context and their prompts are included in the system message.
      *
      * Example:
      * {{{
      * val result = Tool.enable(weatherTool, calculatorTool)(
      *   AI.gen[String]("Plan a trip and calculate expenses.")
      * )
      * }}}
      *
      * @param tools
      *   The tools to enable for the computation
      * @param v
      *   The computation that will have access to the tools
      * @tparam A
      *   The return type of the computation
      * @tparam S
      *   The capability set of the computation
      * @return
      *   The original computation with tools enabled
      */
    def enable[A, S](tools: Tool[S]*)(v: A < S)(using Frame): A < (S & AI) =
        internal.local.update(_ ++ tools.asInstanceOf[Seq[Tool[AI]]])(v)

    /** Combines multiple tools into a single tool.
      *
      * This method allows you to create a composite tool that includes the functionality of multiple individual tools. This is useful for
      * organizing related tools into logical groups or for creating domain-specific toolsets.
      *
      * Example:
      * {{{
      * val weatherTools = Tool.aggregate(getCurrentWeather, getForecast, getHistoricalWeather)
      * val travelTools = Tool.aggregate(findFlights, bookHotel, rentCar)
      * val allTools = Tool.aggregate(weatherTools, travelTools)
      * }}}
      *
      * @param tools
      *   The tools to combine into a single tool
      * @tparam S
      *   The capability set required by the combined tools
      * @return
      *   A new tool that includes all the specified tools
      */
    def aggregate[S](tools: Tool[S]*): Tool[S] =
        new Tool[S]:
            def infos = tools.flatMap(_.infos)

    private[kyo] object internal:

        case class Info[In, Out, -S](
            name: String,
            description: String,
            prompt: Prompt[AI & S],
            run: In => Out < (AI & S),
            inputJson: Json[In],
            outputJson: Json[Out],
            createdAt: Frame
        )

        val local = Local.init(Chunk.empty[Tool[AI]])

        def infos(using Frame): Chunk[Info[?, ?, AI]] < AI =
            local.use(_.flatMap(_.infos))

        def resultTool[T: Json](using Frame): (Tool[AI], Maybe[T] < IO) < AI =
            AtomicRef.init(Maybe.empty[T]).map { ref =>
                val tool =
                    Tool.init[T]("result_tool", "Call this tool with the result.") { result =>
                        ref.set(Present(result))
                    }
                (tool, ref.get)
            }

        def handle(tools: Chunk[Info[?, ?, AI]], calls: List[Call])(using Frame): Unit < AI =
            Kyo.foreachDiscard(calls) { call =>
                tools.find(_.name == call.function) match
                    case None =>
                        AI.updateContext(_.toolMessage(call.id, "Tool not found: " + call.function))
                    case Some(tool) =>
                        val processingMessage = ToolMessage(call.id, "Processing tool call.")
                        Abort.run[AIException] {
                            AI.updateContext(_.add(processingMessage)).andThen {
                                Abort.get(tool.inputJson.decode(call.arguments)).map { x =>
                                    tool.run(x).map(tool.outputJson.encode)
                                }
                            }
                        }.map {
                            case Result.Success(output)                => output
                            case Result.Failure(AIException(error, _)) => "Tool call failure: " + error
                            case Result.Panic(ex)                      => AI.fail(ex.toString())
                        }.map { out =>
                            AI.updateContext { ctx =>
                                ctx.copy(messages = ctx.messages.map {
                                    case `processingMessage` => ToolMessage(call.id, out)
                                    case other               => other
                                })
                            }
                        }
            }
    end internal
end Tool
