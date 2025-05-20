package kyo.ai.completion

import kyo.*
import kyo.AI
import kyo.Tool
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.JsonSchema
import kyo.ai.json.description
import sttp.client3.{Request as _, Response as _, *}
import sttp.client3.ziojson.*
import zio.json.*
import zio.json.ast.Json.Obj
import zio.schema.CaseSet
import zio.schema.DeriveSchema
import zio.schema.Schema as ZSchema
import zio.schema.TypeId
import zio.schema.annotation.discriminatorName
import zio.schema.annotation.noDiscriminator

object AnthropicCompletion extends Completion:

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
        val text = response.content.collect {
            case content if content.`type` == "text" =>
                content.text.getOrElse("")
        }.mkString("")

        val toolCalls = response.content.collect {
            case content if content.`type` == "tool_use" =>
                Call(
                    CallId(content.id.get),
                    content.name.get,
                    content.input.get match
                        case StringValue(value) => value
                        case value: String      => value
                )
        }.toList

        AssistantMessage(text, toolCalls)
    end read

    private def fetch(config: Config, req: Request)(using Frame): Response < (AI & Abort[Throwable]) =
        config.apiKey match
            case Absent => AI.fail("Can't locate API key for model: " + config.model)
            case Present(key) =>
                Requests(
                    _.contentType("application/json")
                        .headers(
                            Map(
                                "x-api-key"         -> key,
                                "anthropic-version" -> "2023-06-01"
                            )
                        )
                        .post(uri"${config.apiUrl}/messages")
                        .body(Json.encode(req))
                        .readTimeout(config.timeout.toScala)
                ).map { r =>
                    Abort.get(Json.decode[Response](stringifyToolInputs(r)))
                }

    private def stringifyToolInputs(json: String): String =
        val toolUsePattern = """"type":"tool_use"[^}]*"input":\s*\{""".r

        var result  = json
        val matches = toolUsePattern.findAllMatchIn(json).toList

        for m <- matches.reverse do
            val inputPrefixPos = result.lastIndexOf("\"input\":", m.end)
            val openBracePos   = result.indexOf("{", inputPrefixPos)

            if openBracePos != -1 then
                var depth    = 1
                var pos      = openBracePos + 1
                var inString = false

                while pos < result.length && depth > 0 do
                    val c = result(pos)

                    if !inString then
                        if c == '{' then depth += 1
                        else if c == '}' then depth -= 1
                        else if c == '"' then inString = true
                    else if c == '"' && (pos == 0 || result(pos - 1) != '\\') then inString = false
                    end if

                    pos += 1
                end while

                if depth == 0 then
                    val closeBracePos = pos - 1
                    val jsonObj       = result.substring(openBracePos, closeBracePos + 1)

                    val escapedJson = jsonObj
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\n", "\\n")

                    result = result.substring(0, openBracePos) +
                        "\"" + escapedJson + "\"" +
                        result.substring(closeBracePos + 1)
                end if
            end if
        end for

        result
    end stringifyToolInputs

    private object internal:

        case class StringValue(value: String)

        case class Content(
            `type`: String,
            text: Option[String] = None,
            id: Option[String] = None,
            name: Option[String] = None,
            input: Option[String | StringValue] = None,
            tool_use_id: Option[String] = None,
            content: Option[String] = None,
            source: Option[Source] = None
        )

        case class Source(
            `type`: String,
            media_type: String,
            data: String
        )

        case class Message(
            role: String,
            content: List[Content]
        )

        case class ToolDefinition(
            name: String,
            description: Option[String],
            input_schema: JsonSchema
        )

        case class Request(
            model: String,
            messages: List[Message],
            system: Option[String],
            max_tokens: Int,
            temperature: Double,
            tools: Option[List[ToolDefinition]] = None
        )

        object Request:
            def apply(
                ctx: Context,
                config: Config,
                tools: Chunk[Tool.internal.Info[?, ?, AI]]
            ): Request =

                val SystemMessage(prompt) = ctx.messages.head

                val messages =
                    ctx.messages.tail.map {
                        case UserMessage(content, Present(image)) =>
                            Message(
                                Role.User.name,
                                List(
                                    Content("text", text = Some(content)),
                                    Content("image", source = Some(Source("base64", "image/jpeg", image.base64)))
                                )
                            )
                        case UserMessage(content, _) =>
                            Message(Role.User.name, List(Content("text", text = Some(content))))
                        case AssistantMessage(content, calls) =>
                            Message(
                                Role.Assistant.name,
                                List(Content("text", text = Some(content))).filter(_.text.exists(_.nonEmpty))
                                    ++ calls.map(call =>
                                        Content(
                                            "tool_use",
                                            id = Some(call.id.id),
                                            name = Some(call.function),
                                            input = Some(StringValue(call.arguments))
                                        )
                                    )
                            )
                        case ToolMessage(callId, content) =>
                            Message(
                                Role.User.name,
                                List(Content("tool_result", tool_use_id = Some(callId.id), content = Some(content)))
                            )
                        case SystemMessage(content) =>
                            Message(Role.User.name, List(Content("text", text = Some(s"[INTERNAL SYSTEM INSTRUCTION] $content"))))
                    }

                val toolDefs =
                    if tools.isEmpty then None
                    else
                        Some(
                            tools.map { tool =>
                                ToolDefinition(
                                    tool.name,
                                    Some(tool.description).filter(_.nonEmpty),
                                    tool.inputJson.jsonSchema
                                )
                            }.toList
                        )

                Request(
                    config.modelName,
                    messages.toList,
                    Some(prompt),
                    config.maxTokens.getOrElse(8192),
                    config.temperature,
                    toolDefs
                )
            end apply
        end Request

        case class Usage(
            input_tokens: Int,
            output_tokens: Int
        )

        case class Response(
            id: String,
            content: List[Content],
            model: String,
            role: String,
            stop_reason: String,
            stop_sequence: Option[String],
            usage: Usage
        )
    end internal
end AnthropicCompletion
