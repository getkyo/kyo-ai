package kyo

import com.fasterxml.jackson.databind.ObjectMapper
import io.modelcontextprotocol.server.McpAsyncServer
import io.modelcontextprotocol.server.McpServer
import io.modelcontextprotocol.server.McpServerFeatures
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import io.modelcontextprotocol.spec.McpSchema
import kyo.ai.json.Json
import reactor.core.publisher.Mono
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

object Mcp:

    object Server:

        def run[S](tools: Tool[S]*)(using Frame): Unit < (Abort[Closed] & Resource & AI & S) =
            run("kyo_mcp_server")(tools*)

        private def run[S](serverName: String, serverVersion: String = "0.1.0")(tools: Tool[S]*)(
            using Frame
        ): Unit < (Abort[Closed] & Resource & AI & S) =

            case class Message(info: Tool.internal.Info[?, ?, S], input: String, promise: Promise[AIException, String])

            for
                infos <- Kyo.foreach(tools)(_.infos).map(_.flatten)

                messages <- Channel.init[Message](Int.MaxValue)

                mcpTools =
                    infos.map { info =>
                        new McpServerFeatures.AsyncToolSpecification(
                            new McpSchema.Tool(
                                info.name,
                                info.description,
                                Json.encode(info.inputJson.jsonSchema)
                            ),
                            (exchange, input) =>
                                val json = new ObjectMapper().writeValueAsString(input)
                                val io =
                                    for
                                        promise <- Promise.init[AIException, String]
                                        _       <- messages.put(Message(info, json, promise))
                                        res     <- promise.getResult
                                    yield res.foldError(
                                        onSuccess = res =>
                                            McpSchema.CallToolResult(
                                                java.util.List.of(McpSchema.TextContent(res)),
                                                false
                                            ),
                                        onError = e =>
                                            McpSchema.CallToolResult(
                                                java.util.List.of(McpSchema.TextContent(e.failureOrPanic.toString)),
                                                true
                                            )
                                    )

                                import AllowUnsafe.embrace.danger
                                val fut = io.handle(Async.run).map(_.toFuture).map(_.asJava)
                                Mono.fromCompletionStage(IO.Unsafe.evalOrThrow(fut))
                        )
                    }

                server <- IO {
                    McpServer.async(new StdioServerTransportProvider(new ObjectMapper()))
                        .serverInfo(serverName, serverVersion)
                        .capabilities(
                            McpSchema.ServerCapabilities.builder()
                                .tools(true)
                                .build()
                        )
                        .tools(mcpTools.asJava)
                        .build()
                }

                _ <- Resource.ensure(IO(server.close()))
            yield Loop.forever {
                messages.take.map {
                    case msg @ Message(info, input, promise) =>
                        Abort.get(info.inputJson.decode(input)).map { input =>
                            info.run(input).map(info.outputJson.encode)
                        }.handle(Abort.run).map(promise.completeDiscard)
                }
            }
            end for
        end run

    end Server
end Mcp
