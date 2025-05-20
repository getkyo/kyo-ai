package kyo.ai

import Context.*
import kyo.*
import kyo.ai.json.Json

case class Context(messages: Chunk[Message]) derives CanEqual:

    def add(msg: Message): Context =
        Context(messages.append(msg))

    def systemMessage(content: String): Context =
        if content.isBlank then this
        else add(SystemMessage(content))

    def userMessage(content: String, image: Maybe[Image] = Absent): Context =
        if content.isBlank && image.isEmpty then this
        else add(UserMessage(content, image))

    def assistantMessage(content: String, calls: List[Call] = Nil): Context =
        if content.isBlank && calls.isEmpty then this
        else add(AssistantMessage(content, calls))

    def toolMessage(callId: CallId, content: String): Context =
        add(ToolMessage(callId, content))

    def isEmpty: Boolean = messages.isEmpty

    def merge(that: Context): Context =
        val commonPrefix = messages.zip(that.messages).takeWhile(_ == _).size
        Context(messages ++ that.messages.drop(commonPrefix))

end Context

object Context:

    val empty = Context(Chunk.empty)

    enum Role(val name: String) derives CanEqual:
        case System    extends Role("system")
        case User      extends Role("user")
        case Assistant extends Role("assistant")
        case Tool      extends Role("tool")
    end Role

    case class CallId(id: String) derives CanEqual

    case class Call(id: CallId, function: String, arguments: String)

    sealed trait Message(val role: Role) derives CanEqual:
        def content: String

    case class SystemMessage(
        content: String
    ) extends Message(Role.System)

    case class UserMessage(
        content: String,
        image: Maybe[Image]
    ) extends Message(Role.User)

    case class AssistantMessage(
        content: String,
        calls: List[Call] = Nil
    ) extends Message(Role.Assistant)

    case class ToolMessage(
        callId: CallId,
        content: String
    ) extends Message(Role.Tool)
end Context
