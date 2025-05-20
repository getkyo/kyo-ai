package kyo.ai.completion

import kyo.*
import kyo.AI
import kyo.Tool
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.JsonSchema
import sttp.client3.{Request as _, Response as _, *}
import sttp.client3.ziojson.*
import zio.json.*
import zio.json.internal.Write
import zio.schema.DeriveSchema

object OpenAICompletion extends Completion:

    import internal.*

    def apply(
        config: Config,
        context: Context,
        tools: Chunk[Tool.internal.Info[?, ?, AI]]
    )(using Frame): AssistantMessage < (AI & Abort[Throwable]) =
        val req = Request(context, config, tools)
        for
            _        <- Log.debug(Json.encodePretty(req))
            response <- fetch(config, req)
            _        <- Log.debug(Json.encodePretty(response))
            message  <- read(response)
        yield message
        end for
    end apply

    private def read(response: Response)(using Frame): AssistantMessage < AI =
        response.choices.headOption match
            case None =>
                AI.fail("LLM response has no choices! " + Json.encode(response))
            case Some(v) =>
                AssistantMessage(
                    v.message.content.getOrElse(""),
                    v.message.tool_calls.getOrElse(Nil).map(c =>
                        Call(CallId(c.id), c.function.name, c.function.arguments)
                    )
                )

    private def fetch(config: Config, req: Request)(using Frame): Response < (AI & Abort[Throwable]) =
        config.apiKey match
            case Absent => AI.fail("Can't locate API key for model: " + config.model)
            case Present(key) =>
                Requests(
                    _.contentType("application/json")
                        .headers(
                            Map(
                                "Authorization" -> s"Bearer $key"
                            ) ++ config.apiOrg.map("OpenAI-Organization" -> _).toList
                        )
                        .post(uri"${config.apiUrl}/chat/completions")
                        .body(Json.encode(req))
                        .readTimeout(config.timeout.toScala)
                ).map { r =>
                    Abort.get(Json.decode[Response](r))
                }

    private object internal:

        case class Name(name: String)

        case class FunctionCall(arguments: String, name: String)
        case class ToolCall(id: String, function: FunctionCall, `type`: String = "function")

        case class FunctionDef(description: String, name: String, strict: Boolean, parameters: JsonSchema)
        case class ToolDef(function: FunctionDef, `type`: String = "function")

        case class MessageEntry(
            role: String,
            content: Option[String],
            tool_calls: Option[List[ToolCall]],
            tool_call_id: Option[String]
        )

        case class VisionEntry(
            content: List[VisionEntry.Text | VisionEntry.Image],
            role: String = "user"
        )

        object VisionEntry:
            case class ImageUrl(url: String)
            case class Text(text: String, `type`: String = "text")
            case class Image(image_url: ImageUrl, `type`: String = "image_url")
        end VisionEntry

        case class Request(
            model: String,
            temperature: Double,
            max_tokens: Option[Int],
            seed: Option[Int],
            messages: List[MessageEntry | VisionEntry],
            tools: Option[List[ToolDef]],
            tool_choice: String = "required"
        )

        private def toEntry(msg: Message) =
            val toolCalls =
                msg match
                    case msg: AssistantMessage =>
                        Some(
                            msg.calls.map(c =>
                                ToolCall(c.id.id, FunctionCall(c.arguments, c.function))
                            )
                        ).filter(_.nonEmpty)
                    case _ =>
                        None
            val callId =
                msg match
                    case msg: ToolMessage =>
                        Some(msg.callId.id)
                    case _ =>
                        None
            MessageEntry(msg.role.name, Some(msg.content), toolCalls, callId)
        end toEntry

        object Request:
            def apply(
                ctx: Context,
                config: Config,
                tools: Chunk[Tool.internal.Info[?, ?, AI]]
            ): Request =
                val entries = ctx.messages.flatMap {
                    case msg: UserMessage if (msg.image.nonEmpty) =>
                        toEntry(msg) ::
                            VisionEntry(
                                msg.image.map(i => VisionEntry.Image(VisionEntry.ImageUrl(s"data:image/jpeg;base64,${i.base64}"))).toList
                            ) :: Nil
                    case msg => List(toEntry(msg))
                }
                val toolDefs =
                    if tools.isEmpty then
                        None
                    else
                        Some(tools.map(p =>
                            ToolDef(FunctionDef(
                                p.description,
                                p.name,
                                false,
                                p.inputJson.jsonSchema
                            ))
                        ).toList)
                Request(
                    config.modelName,
                    config.temperature,
                    config.maxTokens.toOption,
                    config.seed.toOption,
                    entries.toList,
                    toolDefs
                )
            end apply
        end Request

        case class Choice(message: MessageEntry)
        case class Response(choices: List[Choice])

    end internal
end OpenAICompletion
